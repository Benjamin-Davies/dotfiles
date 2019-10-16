ZSH="$HOME/.local/oh-my-zsh"
ZSH_CUSTOM="$HOME/.local/zsh-custom"
ZSH_THEME="theunraveler"

COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(
  git
  npm
  tmux
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

alias cdn='cd ~/Documents/notes; cd'
alias cos='sftp -P 2222 php.mmc.school.nz:/201BH/benjamindavies' # School server
alias pd='pandoc --variable=fontfamily:arev --variable=geometry:margin=2cm'
alias pls=sudo
alias py=python
alias pyhs='python -m http.server 8080'
alias pym='python -m'
alias tm='tmux'

# https://youtu.be/tBoLDpTWVOM
alias config='git --git-dir=$HOME/dotfiles.git/ --work-tree=$HOME'

if command -v xdg-open > /dev/null; then
  alias open='xdg-open'
fi

if command -v nvim > /dev/null; then
  export EDITOR=$(which nvim)
  alias :e=nvim
else
  export EDITOR=$(which vim)
  alias :e=vim
fi

if command -v emacsclient > /dev/null; then
  export ALTERNATE_EDITOR=$EDITOR
  export EDITOR=$(which emacsclient)
  alias :e=emacsclient
fi

# Function to bulk convert md to pdf
pdpdf() {
  for file in "$@"; do
    pandoc "$file" -o "$file".pdf -V header-includes:'\renewcommand{\familydefault}{\sfdefault}'
  done
}

if [ "$TMUX" -o "$EMACS" ]; then
  ~/.local/pfetch/pfetch
else
  # -u flag says to assume utf8 support
  # It was acting unusual over ssh

  # Try to connect to an existing session.
  tmux attach

  # If failed, just run tmux
  if [[ $? -ne 0 ]]; then
    tmux -u
  fi

  # Close zsh
  if [[ $? -eq 0 ]]; then exit; fi
fi
