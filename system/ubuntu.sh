set -e

declare -a programs=(
    "lolcat"
    "cowsay"
    "fortune"
    "toilet"
    "boxes"
    "w3m-img"
    # "bb"
)

echo "Installing: ${programs[@]}"

sudo apt install -y ${programs[@]}

declare -a pyprograms=(
    "glances"
    "doge"
    "ranger-fm"
    "youtube-dl"
    "virtualenvwrapper"
    "httpie"
    "pgcli"
    "iredis"
)

pip3 install --user ${pyprograms[@]}
