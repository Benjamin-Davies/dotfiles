ZSH="$HOME/.local/oh-my-zsh"
ZSH_CUSTOM="$HOME/.local/zsh-custom"
ZSH_THEME="theunraveler"

COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(
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

# https://youtu.be/tBoLDpTWVOM
alias config='git --git-dir=$HOME/dotfiles.git/ --work-tree=$HOME'
alias cos='sftp -P 2222 php.mmc.school.nz:/201BH/benjamindavies' # School server
alias :e=$EDITOR

if command -v xdg-open > /dev/null; then
  alias open='xdg-open'
fi

if command -v nvim > /dev/null; then
  export EDITOR=$(which nvim)
else
  export EDITOR=$(which vim)
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
