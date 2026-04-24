#!/usr/bin/env python3
# /// script
# dependencies = [
#     "rich",
# ]
# ///

import subprocess
import sys
from datetime import datetime, timedelta
from collections import defaultdict
from rich.console import Console
from rich.table import Table
from rich import box

def get_git_commits_last_30_days():
    """Get git commits from the last 30 days with numstat"""
    try:
        # Calculate date 30 days ago
        thirty_days_ago = datetime.now() - timedelta(days=30)
        since_date = thirty_days_ago.strftime("%Y-%m-%d")
        
        # Run git log command with numstat to get line changes
        cmd = [
            "git", "log", 
            f"--since={since_date}",
            "--pretty=format:COMMIT_START|%an|%ad|%s|COMMIT_END",
            "--numstat",
            "--date=short"
        ]
        
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        return result.stdout.strip() if result.stdout.strip() else ""
        
    except subprocess.CalledProcessError as e:
        console = Console()
        console.print(f"❌ Error running git command: {e}", style="red")
        console.print("Make sure you're in a git repository", style="yellow")
        sys.exit(1)
    except FileNotFoundError:
        console = Console()
        console.print("❌ Git is not installed or not in PATH", style="red")
        sys.exit(1)

def normalize_author_name(author):
    """Normalize author name for comparison"""
    # Remove email addresses and extra whitespace
    import re
    author = re.sub(r'<[^>]+>', '', author).strip()
    
    # Convert to lowercase for comparison
    normalized = author.lower().strip()
    
    # Remove common suffixes/prefixes that might vary
    normalized = re.sub(r'\b(jr\.?|sr\.?|ii|iii|iv)\b', '', normalized).strip()
    
    return normalized

def extract_first_name(author):
    """Extract first name from author string"""
    normalized = normalize_author_name(author)
    
    # Split by whitespace and take first part
    parts = normalized.split()
    if parts:
        return parts[0]
    return normalized

def merge_similar_authors(author_commits):
    """Merge authors with similar names"""
    console = Console()
    
    # Create mapping of normalized names to original names
    name_groups = defaultdict(list)
    
    # Group authors by their first name
    for author in author_commits.keys():
        first_name = extract_first_name(author)
        name_groups[first_name].append(author)
    
    merged_commits = defaultdict(list)
    author_mapping = {}
    
    for first_name, authors in name_groups.items():
        if len(authors) == 1:
            # Single author with this first name, no merging needed
            primary_author = authors[0]
            merged_commits[primary_author] = author_commits[primary_author]
            author_mapping[primary_author] = primary_author
        else:
            # Multiple authors with same first name - need to decide which to merge
            
            # Sort by length (longer names usually more complete)
            authors.sort(key=len, reverse=True)
            primary_author = authors[0]
            
            merged_authors = [primary_author]
            
            # Check if shorter names are substrings of longer names
            for other_author in authors[1:]:
                other_normalized = normalize_author_name(other_author)
                primary_normalized = normalize_author_name(primary_author)
                
                # If the shorter name is contained in the longer name, merge them
                if (other_normalized in primary_normalized or 
                    primary_normalized in other_normalized or
                    # Also check if they have the same first name and one is just first name
                    (len(other_normalized.split()) == 1 and 
                     other_normalized == extract_first_name(primary_author))):
                    merged_authors.append(other_author)
                    author_mapping[other_author] = primary_author
                else:
                    # Keep as separate author
                    merged_commits[other_author] = author_commits[other_author]
                    author_mapping[other_author] = other_author
            
            # Merge commits for similar authors
            if len(merged_authors) > 1:
                console.print(f"🔗 Merging authors: {', '.join(merged_authors)} → {primary_author}", style="dim yellow")
                
                for author in merged_authors:
                    merged_commits[primary_author].extend(author_commits[author])
                    author_mapping[author] = primary_author
            else:
                merged_commits[primary_author] = author_commits[primary_author]
                author_mapping[primary_author] = primary_author
    
    return merged_commits, author_mapping

def parse_commits(git_output):
    """Parse commit output with numstat and group by author"""
    if not git_output:
        return {}
    
    author_commits = defaultdict(list)
    
    # Split by commits
    commits = git_output.split('COMMIT_START|')[1:]  # Remove empty first element
    
    for commit_block in commits:
        if not commit_block.strip():
            continue
            
        lines = commit_block.split('\n')
        if not lines:
            continue
            
        # Parse commit header
        header_line = lines[0]
        if '|COMMIT_END' not in header_line:
            continue
            
        header_parts = header_line.split('|COMMIT_END')[0].split('|')
        if len(header_parts) < 3:
            continue
            
        author = header_parts[0].strip()
        date = header_parts[1].strip()
        subject = '|'.join(header_parts[2:]).strip()  # Join in case subject contains |
        
        # Parse numstat data
        total_insertions = 0
        total_deletions = 0
        files_changed = 0
        
        # Look for numstat lines after the commit header
        numstat_lines = [line for line in lines[1:] if line.strip() and '\t' in line]
        
        for numstat_line in numstat_lines:
            parts = numstat_line.split('\t')
            if len(parts) >= 3:
                try:
                    insertions = int(parts[0]) if parts[0] != '-' else 0
                    deletions = int(parts[1]) if parts[1] != '-' else 0
                    # parts[2] is the filename
                    
                    total_insertions += insertions
                    total_deletions += deletions
                    files_changed += 1
                except ValueError:
                    # Skip lines that don't have valid numbers (binary files show '-')
                    files_changed += 1
        
        author_commits[author].append({
            'date': date,
            'subject': subject,
            'insertions': total_insertions,
            'deletions': total_deletions,
            'files_changed': files_changed,
            'net_changes': total_insertions - total_deletions
        })
    
    # Merge similar authors
    merged_commits, author_mapping = merge_similar_authors(author_commits)
    
    return merged_commits

def display_line_change_stats(author_commits):
    """Display line change statistics per author"""
    console = Console()
    
    if not author_commits:
        return
    
    # Calculate stats per author
    author_stats = []
    for author, commits in author_commits.items():
        total_insertions = sum(commit['insertions'] for commit in commits)
        total_deletions = sum(commit['deletions'] for commit in commits)
        total_files = sum(commit['files_changed'] for commit in commits)
        net_changes = total_insertions - total_deletions
        commit_count = len(commits)
        avg_lines_per_commit = (total_insertions + total_deletions) / commit_count if commit_count > 0 else 0
        
        author_stats.append({
            'author': author,
            'commits': commit_count,
            'insertions': total_insertions,
            'deletions': total_deletions,
            'net_changes': net_changes,
            'files_changed': total_files,
            'avg_lines_per_commit': avg_lines_per_commit,
            'total_lines_changed': total_insertions + total_deletions
        })
    
    # Sort by total lines changed
    author_stats.sort(key=lambda x: x['total_lines_changed'], reverse=True)
    
    console.print("\n📊 Line Changes by Author - Last 30 Days", style="bold blue")
    console.print("=" * 60, style="blue")
    
    # Create detailed table
    table = Table(box=box.ROUNDED, show_header=True, header_style="bold magenta")
    table.add_column("👤 Author", style="cyan", no_wrap=True)
    table.add_column("🔢 Commits", style="white", justify="right")
    table.add_column("➕ Added", style="green", justify="right")
    table.add_column("➖ Deleted", style="red", justify="right")
    table.add_column("📈 Net", style="yellow", justify="right")
    table.add_column("📁 Files", style="blue", justify="right")
    table.add_column("📊 Avg/Commit", style="magenta", justify="right")
    
    for stats in author_stats:
        # Color code net changes
        net_color = "green" if stats['net_changes'] >= 0 else "red"
        net_sign = "+" if stats['net_changes'] >= 0 else ""
        
        table.add_row(
            stats['author'],
            str(stats['commits']),
            f"+{stats['insertions']:,}",
            f"-{stats['deletions']:,}",
            f"[{net_color}]{net_sign}{stats['net_changes']:,}[/{net_color}]",
            str(stats['files_changed']),
            f"{stats['avg_lines_per_commit']:.1f}"
        )
    
    console.print(table)
    
    # Summary of line changes
    total_insertions = sum(stats['insertions'] for stats in author_stats)
    total_deletions = sum(stats['deletions'] for stats in author_stats)
    total_net = total_insertions - total_deletions
    total_files = sum(stats['files_changed'] for stats in author_stats)
    
    console.print("\n📋 Line Change Summary:", style="bold cyan")
    summary_table = Table(box=box.SIMPLE, show_header=False)
    summary_table.add_column("Metric", style="yellow")
    summary_table.add_column("Value", style="white")
    
    summary_table.add_row("➕ Total lines added:", f"+{total_insertions:,}")
    summary_table.add_row("➖ Total lines deleted:", f"-{total_deletions:,}")
    
    net_color = "green" if total_net >= 0 else "red"
    net_sign = "+" if total_net >= 0 else ""
    summary_table.add_row("📈 Net change:", f"[{net_color}]{net_sign}{total_net:,}[/{net_color}]")
    summary_table.add_row("📁 Total files touched:", f"{total_files:,}")
    
    # Find most productive author
    if author_stats:
        most_productive = author_stats[0]
        summary_table.add_row("🏆 Most active (by lines):", f"{most_productive['author']} ({most_productive['total_lines_changed']:,} lines)")
    
    console.print(summary_table)

def display_commit_graph(author_commits):
    """Display commit statistics in a nice terminal graph"""
    console = Console()
    
    if not author_commits:
        console.print("📭 No commits found in the last 30 days", style="yellow")
        return
    
    # Sort authors by commit count
    sorted_authors = sorted(author_commits.items(), key=lambda x: len(x[1]), reverse=True)
    
    console.print("\n🚀 Git Commit Activity - Last 30 Days", style="bold blue")
    console.print("=" * 50, style="blue")
    
    # Create a table
    table = Table(box=box.ROUNDED, show_header=True, header_style="bold magenta")
    table.add_column("👤 Author", style="cyan", no_wrap=True)
    table.add_column("📈 Commits", style="magenta", justify="right")
    table.add_column("📊 Graph", style="green", min_width=45)
    
    max_commits = max(len(commits) for commits in author_commits.values()) if author_commits else 1
    
    for author, commits in sorted_authors:
        commit_count = len(commits)
        # Create a simple bar graph using characters
        bar_length = int((commit_count / max_commits) * 35) if max_commits > 0 else 0
        bar = "█" * bar_length
        padding = " " * (35 - bar_length)
        
        table.add_row(
            author,
            str(commit_count),
            f"[green]{bar}[/green]{padding} {commit_count}"
        )
    
    console.print(table)
    
    # Summary statistics
    total_commits = sum(len(commits) for commits in author_commits.values())
    total_authors = len(author_commits)
    avg_commits = total_commits / total_authors if total_authors > 0 else 0
    
    console.print("\n📊 Summary Statistics:", style="bold cyan")
    summary_table = Table(box=box.SIMPLE, show_header=False)
    summary_table.add_column("Metric", style="yellow")
    summary_table.add_column("Value", style="white")
    
    summary_table.add_row("📝 Total commits:", str(total_commits))
    summary_table.add_row("👥 Total authors:", str(total_authors))
    summary_table.add_row("📊 Avg commits/author:", f"{avg_commits:.1f}")
    
    # Find most active day
    all_dates = []
    for commits in author_commits.values():
        for commit in commits:
            all_dates.append(commit['date'])
    
    if all_dates:
        from collections import Counter
        date_counts = Counter(all_dates)
        most_active_date, most_active_count = date_counts.most_common(1)[0]
        summary_table.add_row("🔥 Most active date:", f"{most_active_date} ({most_active_count} commits)")
    
    console.print(summary_table)

def display_recent_commits(author_commits, limit=5):
    """Display recent commits by each author"""
    console = Console()
    
    if not author_commits:
        return
    
    console.print(f"\n📋 Recent Commits by Author (showing up to {limit} per author):", style="bold cyan")
    
    for author, commits in sorted(author_commits.items(), key=lambda x: len(x[1]), reverse=True):
        console.print(f"\n👤 {author} ({len(commits)} total commits):", style="bold yellow")
        
        # Sort commits by date (most recent first) and show up to limit
        recent_commits = sorted(commits, key=lambda x: x['date'], reverse=True)[:limit]
        
        for i, commit in enumerate(recent_commits, 1):
            # Show line changes if available
            line_info = ""
            if 'insertions' in commit and 'deletions' in commit:
                total_changes = commit['insertions'] + commit['deletions']
                if total_changes > 0:
                    line_info = f" [green]+{commit['insertions']}[/green]/[red]-{commit['deletions']}[/red]"
            
            console.print(f"  {i}. [{commit['date']}] {commit['subject']}{line_info}", style="dim white")

def main():
    console = Console()
    
    console.print("🔍 Analyzing git commit history for the last 30 days...", style="yellow")
    
    # Get commits with numstat
    git_output = get_git_commits_last_30_days()
    
    if not git_output:
        console.print("📭 No commits found in the last 30 days", style="yellow")
        return
    
    # Parse and group by author (with merging and line stats)
    author_commits = parse_commits(git_output)
    
    if not author_commits:
        console.print("📭 No commits found in the last 30 days", style="yellow")
        return
    
    # Display the commit count graph
    display_commit_graph(author_commits)
    
    # Display line change statistics
    display_line_change_stats(author_commits)
    
    # Display recent commits
    display_recent_commits(author_commits)
    
    console.print("\n✨ Analysis complete!", style="bold green")

if __name__ == "__main__":
    main()
