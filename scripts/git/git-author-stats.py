#!/usr/bin/env python3
import os
import sys
import subprocess
import collections

def author_counts_since(days: int) -> collections.Counter:
    since = f"{days} days ago"
    cmd = ["git", "log", f"--since={since}", "--format=%an"]
    try:
        output = subprocess.check_output(cmd, cwd=os.getcwd(),
                                         stderr=subprocess.STDOUT,
                                         text=True)
    except subprocess.CalledProcessError as exc:
        print(f"Error running git: {exc.output.strip()}", file=sys.stderr)
        sys.exit(1)
    authors = [line for line in output.splitlines() if line]
    return collections.Counter(authors)

def merge_authors(counter: collections.Counter) -> collections.Counter:
    merged = collections.Counter()
    for name, count in counter.items():
        key = name.lower()[:5]
        merged[key] += count
    return merged

def main():
    days_list = [7, 14, 21, 28]
    print(f"{'Days':>4} | {'Author':<30} | {'Commits'}")
    print("-" * 45)
    for days in days_list:
        raw_counts = author_counts_since(days)
        merged_counts = merge_authors(raw_counts)
        if not merged_counts:
            print(f"{days:>4} | {'(none)':<30} | 0")
            continue
        for key, n in merged_counts.most_common():
            print(f"{days:>4} | {key:<30} | {n:>7}")

if __name__ == "__main__":
    main()
