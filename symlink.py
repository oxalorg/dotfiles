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
"""

import argparse
import configparser
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


def load_config(config_path: Path) -> configparser.ConfigParser:
    """Load and parse the symlink.ini configuration file."""
    if not config_path.exists():
        print(f"ERROR: Config file not found: {config_path}", file=sys.stderr)
        sys.exit(1)

    config = configparser.ConfigParser()
    config.read(config_path)
    return config


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
        if "source" not in config[section]:
            print(f"WARNING: Section [{section}] missing source, skipping", file=sys.stderr)
            errors += 1
            continue

        # Look for platform-specific target first, then fall back to default
        target_key_platform = f"target_{current_platform}"
        if target_key_platform in config[section]:
            target_str = config[section][target_key_platform]
        elif "target" in config[section]:
            target_str = config[section]["target"]
        else:
            print(f"WARNING: Section [{section}] has no target for {current_platform}, skipping", file=sys.stderr)
            errors += 1
            continue

        source_rel = config[section]["source"]

        # Source is relative to dotfiles root
        source = (dotfiles_root / source_rel).resolve()
        # Target can use ~ and env vars
        target = expand_path(target_str)

        if create_symlink(source, target, dry_run=args.dry_run, verbose=args.verbose):
            changes += 1

    print(f"\n{'Would make' if args.dry_run else 'Made'} {changes} change(s)")
    if errors:
        print(f"{errors} error(s) encountered")
        sys.exit(1)


if __name__ == "__main__":
    main()
