ZSH="$HOME/.local/oh-my-zsh"
ZSH_CUSTOM="$HOME/.local/zsh-custom"

COMPLETION_WAITING_DOTS="true"
DISABLE_AUTO_UPDATE="true"

plugins=(
  docker
  docker-compose
  git
  vi-mode
  zsh-autosuggestions
  zsh-syntax-highlighting
)

source $ZSH/oh-my-zsh.sh

# Directories for programs
export PATH="$HOME/.local/bin:$HOME/.deno/bin:./node_modules/.bin:$PATH"
# If in doubt use good old fashioned VIM
export EDITOR=$(which vim)
# Very minimal prompt: > or # for root
export prompt='
%(!.#.>) '
# Use light grey for autosuggestions
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=7'
# Hide untracked files in Git
export DISABLE_UNTRACKED_FILES_DIRTY='true'

# open is a useful command on macOS
# Lets get it on Linux as well
if command -v xdg-open > /dev/null; then
  alias open='xdg-open'
fi

# Command to access my dotfiles repo
# Stored in ~/dotfiles.git to avoid being seen by git all the time
alias cfg='git --git-dir=$HOME/dotfiles.git/ --work-tree=$HOME'
# Our school SFTP server is kinda fussy
alias cos='sftp -P 2222 php.mmc.school.nz:/201COS/benjamindavies'
# More docker-compose aliases
alias dcu='docker-compose up --build'
alias dcdu='docker-compose -f docker-compose.yml -f docker-compose.dev.yml up --build'
# Restart ZSH (keeps env)
alias restart='clear && exec zsh'

# Function to bulk convert md to pdf
pdpdf() {
  for file in "$@"; do
    pandoc "$file" -o "$file".pdf -V header-includes:'\renewcommand{\familydefault}{\sfdefault}'
  done
}

# Some distros do weird things
# Use default colors for ls
export LSCOLORS=
export LS_COLORS=
# Yes, I am from New Zealand
if [ -z $LC_CTYPE ]; then
  export LC_CTYPE=en_NZ.UTF-8
fi
# Skip compression for AUR packages
export PKGEXT=.pkg.tar
# Disable macOS shell resume
export SHELL_SESSIONS_DISABLE=1

# Lets show some pretty stuff
myfetch
