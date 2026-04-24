#!/usr/bin/env python3
"""
Idempotently symlink dotfiles based on symlink.ini configuration.

The symlink.ini file should have sections where each section represents
a symlink mapping:

[zshrc]
source = shell/zshrc
target = ~/.zshrc

[nvim]
source = nvim
target = ~/.config/nvim

Platform-specific targets are supported:
- target: default (used on Linux)
- target_macos: used on macOS (falls back to target if not specified)

[some-config]
source = config
target = ~/.config/app
target_macos = ~/Library/Application Support/app

Flatten mode: walk a source directory recursively and create one symlink
per file, flattened by basename, under a target directory:

[scripts-bin]
mode = flatten
source = scripts
target = ~/.local/bin
exclude = README.md, *archived*     ; fnmatch patterns, matched against basename and relative path
executable = true                   ; chmod u+x each source file
skip_macos = pbcopy, pbpaste        ; basenames to skip on macOS (also skip_linux)
prune = true                        ; remove broken symlinks in the target dir
"""

import argparse
import configparser
import fnmatch
import os
import platform
import sys
from pathlib import Path


def get_platform() -> str:
    """Get the current platform: 'linux' or 'macos'."""
    system = platform.system().lower()
    if system == "darwin":
        return "macos"
    return "linux"


def expand_path(path: str) -> Path:
    """Expand ~ and environment variables in path."""
    return Path(os.path.expandvars(os.path.expanduser(path)))


def get_dotfiles_root() -> Path:
    """Get the root directory of the dotfiles repo (where this script lives)."""
    return Path(__file__).parent.resolve()


def create_symlink(source: Path, target: Path, dry_run: bool = False, verbose: bool = False) -> bool:
    """
    Create a symlink from target to source idempotently.

    Returns True if a change was made, False otherwise.
    """
    # Ensure source exists
    if not source.exists():
        print(f"ERROR: Source does not exist: {source}", file=sys.stderr)
        return False

    # Check if target already exists
    if target.is_symlink():
        current_target = target.resolve()
        if current_target == source:
            if verbose:
                print(f"OK: {target} -> {source} (already linked)")
            return False
        else:
            # Symlink exists but points elsewhere
            if dry_run:
                print(f"WOULD UPDATE: {target} -> {source} (currently -> {current_target})")
            else:
                target.unlink()
                target.symlink_to(source)
                print(f"UPDATED: {target} -> {source} (was -> {current_target})")
            return True
    elif target.exists():
        # Target exists but is not a symlink (regular file/directory)
        print(f"WARNING: {target} exists and is not a symlink, skipping", file=sys.stderr)
        return False
    else:
        # Target doesn't exist, create parent dirs if needed and symlink
        if dry_run:
            print(f"WOULD CREATE: {target} -> {source}")
        else:
            target.parent.mkdir(parents=True, exist_ok=True)
            target.symlink_to(source)
            print(f"CREATED: {target} -> {source}")
        return True


def is_excluded(rel_path: Path, patterns: list[str]) -> bool:
    """Return True if rel_path matches any fnmatch pattern (against basename or full relative path)."""
    basename = rel_path.name
    rel_str = str(rel_path)
    for pattern in patterns:
        if fnmatch.fnmatch(basename, pattern) or fnmatch.fnmatch(rel_str, pattern):
            return True
    return False


def collect_flatten_files(
    source_dir: Path, exclude: list[str], skip: list[str]
) -> tuple[dict[str, Path], dict[str, list[Path]]]:
    """
    Walk source_dir and return ({basename: source_path}, {basename: [colliding_paths]}).

    Files matching any exclude pattern or whose basename is in skip are omitted.
    Basename collisions are reported separately and removed from the accepted map.
    """
    accepted: dict[str, Path] = {}
    collisions: dict[str, list[Path]] = {}

    for root, _dirs, files in os.walk(source_dir):
        for name in files:
            full = Path(root) / name
            rel = full.relative_to(source_dir)
            if is_excluded(rel, exclude):
                continue
            if name in skip:
                continue
            if name in accepted:
                collisions.setdefault(name, [accepted[name]]).append(full)
            else:
                accepted[name] = full

    for name in collisions:
        accepted.pop(name, None)

    return accepted, collisions


def prune_broken_symlinks(target_dir: Path, dry_run: bool) -> int:
    """Remove broken symlinks in target_dir. Returns number of links pruned."""
    if not target_dir.is_dir():
        return 0
    pruned = 0
    for entry in target_dir.iterdir():
        if entry.is_symlink() and not entry.exists():
            if dry_run:
                print(f"WOULD PRUNE: {entry} (broken symlink)")
            else:
                entry.unlink()
                print(f"PRUNED: {entry} (broken symlink)")
            pruned += 1
    return pruned


def parse_list(value: str) -> list[str]:
    """Parse a comma-separated INI value into a stripped list."""
    return [item.strip() for item in value.split(",") if item.strip()]


def load_config(config_path: Path) -> configparser.ConfigParser:
    """Load and parse the symlink.ini configuration file."""
    if not config_path.exists():
        print(f"ERROR: Config file not found: {config_path}", file=sys.stderr)
        sys.exit(1)

    config = configparser.ConfigParser()
    config.read(config_path)
    return config


def process_flatten(
    section_name: str,
    section,
    source_dir: Path,
    target_dir: Path,
    current_platform: str,
    dry_run: bool,
    verbose: bool,
) -> tuple[int, int]:
    """Handle a mode=flatten section. Returns (changes, errors)."""
    if not source_dir.is_dir():
        print(f"ERROR: [{section_name}] source is not a directory: {source_dir}", file=sys.stderr)
        return 0, 1

    exclude = parse_list(section.get("exclude", ""))
    skip = parse_list(section.get(f"skip_{current_platform}", ""))
    executable = section.getboolean("executable", fallback=False)
    prune = section.getboolean("prune", fallback=False)

    accepted, collisions = collect_flatten_files(source_dir, exclude, skip)

    errors = 0
    for name, paths in collisions.items():
        paths_str = ", ".join(str(p.relative_to(source_dir)) for p in paths)
        print(f"ERROR: [{section_name}] basename collision '{name}' from: {paths_str} — skipping", file=sys.stderr)
        errors += 1

    changes = 0
    if prune:
        changes += prune_broken_symlinks(target_dir, dry_run=dry_run)

    for name in sorted(accepted):
        src = accepted[name]
        tgt = target_dir / name

        if executable and not dry_run:
            mode_bits = src.stat().st_mode
            if not (mode_bits & 0o100):
                src.chmod(mode_bits | 0o100)

        if create_symlink(src, tgt, dry_run=dry_run, verbose=verbose):
            changes += 1

    return changes, errors


def main():
    parser = argparse.ArgumentParser(
        description="Idempotently symlink dotfiles based on symlink.ini"
    )
    parser.add_argument(
        "-c", "--config",
        default="symlink.ini",
        help="Path to config file (default: symlink.ini in script directory)"
    )
    parser.add_argument(
        "-n", "--dry-run",
        action="store_true",
        help="Show what would be done without making changes"
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Show status for all symlinks, including unchanged ones"
    )
    args = parser.parse_args()

    dotfiles_root = get_dotfiles_root()

    # Resolve config path relative to dotfiles root if not absolute
    config_path = Path(args.config)
    if not config_path.is_absolute():
        config_path = dotfiles_root / config_path

    config = load_config(config_path)

    current_platform = get_platform()

    if args.dry_run:
        print("=== DRY RUN MODE ===\n")

    print(f"Platform: {current_platform}\n")

    changes = 0
    errors = 0

    for section in config.sections():
        sec = config[section]
        if "source" not in sec:
            print(f"WARNING: Section [{section}] missing source, skipping", file=sys.stderr)
            errors += 1
            continue

        target_key_platform = f"target_{current_platform}"
        if target_key_platform in sec:
            target_str = sec[target_key_platform]
        elif "target" in sec:
            target_str = sec["target"]
        else:
            print(f"WARNING: Section [{section}] has no target for {current_platform}, skipping", file=sys.stderr)
            errors += 1
            continue

        source = (dotfiles_root / sec["source"]).resolve()
        target = expand_path(target_str)
        mode = sec.get("mode", "link").strip().lower()

        if mode == "link":
            if create_symlink(source, target, dry_run=args.dry_run, verbose=args.verbose):
                changes += 1
        elif mode == "flatten":
            sec_changes, sec_errors = process_flatten(
                section, sec, source, target, current_platform,
                dry_run=args.dry_run, verbose=args.verbose,
            )
            changes += sec_changes
            errors += sec_errors
        else:
            print(f"WARNING: Section [{section}] has unknown mode '{mode}', skipping", file=sys.stderr)
            errors += 1

    print(f"\n{'Would make' if args.dry_run else 'Made'} {changes} change(s)")
    if errors:
        print(f"{errors} error(s) encountered")
        sys.exit(1)


if __name__ == "__main__":
    main()
