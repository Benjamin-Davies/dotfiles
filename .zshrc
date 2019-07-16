export ZSH="$HOME/.oh-my-zsh"

if [[ ! -d "$ZSH" ]]; then
  git clone --depth=1 https://github.com/robbyrussell/oh-my-zsh $ZSH
fi

ZSH_AUTOSUGGESTIONS=${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
if [[ ! -d "$ZSH_AUTOSUGGESTIONS" ]]; then
  git clone --depth=1 https://github.com/zsh-users/zsh-autosuggestions $ZSH_AUTOSUGGESTIONS
fi

ZSH_THEME="cypher"

COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(
  vi-mode
  git
  npm
  tmux
  zsh-autosuggestions
)

source $ZSH/oh-my-zsh.sh

if [[ "$OSTYPE" == "darwin"* ]]; then
  export GEM_HOME=$HOME/.gem
  export PATH="$HOME/.gem/bin:$PATH"
else
  export PATH="$HOME/.gem/ruby/2.6.0/bin:$PATH"
fi

export PATH="/snap/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/.cabal/bin:$PATH"
export EDITOR=$(which nvim)

alias cdn='cd ~/Documents/notes; cd'
alias cos='sftp -P 2222 php.mmc.school.nz:/201BH/benjamindavies' # School server
alias pd='pandoc --variable=fontfamily:arev --variable=geometry:margin=2cm'
alias py=python
alias pyhs='python -m http.server 8080'
alias pym='python -m'
alias tm='tmux'
alias nv='nvim'

# https://youtu.be/tBoLDpTWVOM
alias config='/usr/bin/git --git-dir=$HOME/dotfiles.git/ --work-tree=$HOME'

if [ "$TMUX" ]; then
  clear
  neofetch
fi
