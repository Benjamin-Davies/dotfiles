export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(
  vi-mode
  git
  npm
  tmux
)

source $ZSH/oh-my-zsh.sh

if [[ "$OSTYPE" == "darwin"* ]]; then
  export GEM_HOME=$HOME/.gem
  export PATH="$HOME/.gem/bin:$PATH"
else
  export PATH="$HOME/.gem/ruby/2.6.0/bin:$PATH"
fi

export PATH="/snap/bin:$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
export EDITOR=$(which nvim)
# Replace unicode arrow at start of prompt with ->
export ret_status="%(?:%{$fg_bold[green]%}->:%{$fg_bold[red]%}->)"

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

if [ "$TERM" = "screen" ]; then
  neofetch
fi
