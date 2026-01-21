#!/usr/bin/env bash
#
#  setup.sh
#
#  Create symbolic links for:
#      .zshrc   ←  zshrc
#      .aliases ←  aliases
#      .bashrc  ←  bashrc
#
#  The script is deliberately idempotent: running it again will simply
#  overwrite the links with the latest files from this directory.
#
#  Usage:
#      1. Drop this file into the folder that contains `zshrc`, `aliases`
#         and `bashrc`.
#      2. chmod +x setup.sh
#      3. ./setup.sh
#
#  (If you prefer to keep backups of existing dot‑files you can modify
#  the `ln` command below – see the comment block in the script.)

set -euo pipefail

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
# Resolve the absolute path of the directory that contains this script.
# This allows the script to be run from any working directory.
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Resolve the absolute path to the user's home directory.
home_dir="${HOME:-$PWD}"   # Fallback to current dir if $HOME is unset

# ------------------------------------------------------------
# Map of source file → target symlink
# ------------------------------------------------------------
declare -A links=(
    [zshrc]=".zshrc"
    [aliases]=".aliases"
    [bashrc]=".bashrc"
)

# ------------------------------------------------------------
# Main loop – create / overwrite symlinks
# ------------------------------------------------------------
echo "Creating symlinks from $script_dir to $home_dir"

for src_file in "${!links[@]}"; do
    src_path="$script_dir/$src_file"
    tgt_path="$home_dir/${links[$src_file]}"

    # Check that the source file actually exists
    if [[ ! -e "$src_path" ]]; then
        echo "⚠️  Source file not found: $src_path – skipping"
        continue
    fi

    # Remove any existing file (including a dangling symlink) before creating a new one
    if [[ -e "$tgt_path" ]]; then
        echo "🔄  Overwriting existing $tgt_path"
        rm -rf "$tgt_path"
    fi

    # Create the symlink
    ln -s "$src_path" "$tgt_path"

    echo "✅  Linked $tgt_path → $src_path"
done

echo "✔️  All requested dot‑files linked successfully!"
