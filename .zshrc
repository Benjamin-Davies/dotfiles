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

export PATH="$HOME/.local/bin:$HOME/.deno/bin:./node_modules/.bin:$PATH"
export EDITOR=$(which vim)

# Use default colors for ls
export LSCOLORS=
export LS_COLORS=
# And light grey for autosuggestions
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=7'
# Skip compression for AUR packages
export PKGEXT=.pkg.tar

if command -v xdg-open > /dev/null; then
  alias open='xdg-open'
fi

if [ -z $LC_CTYPE ]; then
  export LC_CTYPE=en_NZ.UTF-8
fi

# https://youtu.be/tBoLDpTWVOM
alias cfg='git --git-dir=$HOME/dotfiles.git/ --work-tree=$HOME'
alias cos='sftp -P 2222 php.mmc.school.nz:/201COS/benjamindavies' # School server
alias dcu='docker-compose up --build'
alias dcdu='docker-compose -f docker-compose.yml -f docker-compose.dev.yml up --build'
alias restart='clear && exec zsh'

# Function to bulk convert md to pdf
pdpdf() {
  for file in "$@"; do
    pandoc "$file" -o "$file".pdf -V header-includes:'\renewcommand{\familydefault}{\sfdefault}'
  done
}

myfetch
