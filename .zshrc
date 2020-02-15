ZSH="$HOME/.local/oh-my-zsh"
ZSH_CUSTOM="$HOME/.local/zsh-custom"
ZSH_THEME="avit"

COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"
DISABLE_AUTO_UPDATE="true"
DISABLE_AUTO_TITLE="true"

plugins=(
  docker
  docker-compose
  git
  vi-mode
  zsh-autosuggestions
  zsh-syntax-highlighting
)

source $ZSH/oh-my-zsh.sh

if [[ "$OSTYPE" == "darwin"* ]]; then
  export GEM_HOME=$HOME/.gem
  export PATH="$HOME/.gem/bin:$PATH"
else
  export PATH="$HOME/.gem/ruby/2.6.0/bin:$PATH"
fi

export PATH="$HOME/.local/bin:./node_modules/.bin:$PATH"

# Use default colors for ls
export LSCOLORS=
export LS_COLORS=

if command -v xdg-open > /dev/null; then
  alias open='xdg-open'
fi

if command -v nvr > /dev/null; then
  export EDITOR="$(which nvr) --remote-tab-wait-silent"
elif command -v nvim > /dev/null; then
  export EDITOR=$(which nvim)
else
  export EDITOR=$(which vim)
fi

if [ -z $LC_CTYPE ]; then
  export LC_CTYPE=en_NZ.UTF-8
fi

# https://youtu.be/tBoLDpTWVOM
alias cfg='git --git-dir=$HOME/dotfiles.git/ --work-tree=$HOME'
alias cos='sftp -P 2222 php.mmc.school.nz:/201COS/benjamindavies' # School server
alias gcalcli='gcalcli --client-id 991880063730-lu4otp4132sugbed4ut8adqjdjfnkrqe.apps.googleusercontent.com --client-secret evW8BVRQuAtM4U4XzqRG54iv'
alias restart='clear && exec zsh'

# Function to bulk convert md to pdf
pdpdf() {
  for file in "$@"; do
    pandoc "$file" -o "$file".pdf -V header-includes:'\renewcommand{\familydefault}{\sfdefault}'
  done
}

myfetch
