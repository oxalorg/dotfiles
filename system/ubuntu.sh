set -e

# Installing oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

declare -a programs=(
    "lolcat"
    "cowsay"
    "fortune"
    "toilet"
    "boxes"
    "w3m-img"
    "python3-pip"
    "fonts-firacode"
    "fd-find"
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
