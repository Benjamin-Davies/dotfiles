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

export PATH=$PATH:/snap/bin
export EDITOR=$(which nvim)
# Replace unicode arrow at start of prompt with ->
export ret_status="%(?:%{$fg_bold[green]%}->:%{$fg_bold[red]%}->)"

alias cdn='cd ~/Documents/notes; cd'
alias pd='pandoc --variable=fontfamily:arev --variable=geometry:margin=2cm'
alias py=python
alias pyhs='python -m http.server 8080'
alias pym='python -m'
alias tm=tmux
alias ta='tmux attach || tmux'
alias nv='nvim'

# https://youtu.be/tBoLDpTWVOM
alias config='/usr/bin/git --git-dir=$HOME/dotfiles.git/ --work-tree=$HOME'

if [ -d /mnt/c/Windows/ ]; then
  alias ci='cd /mnt/c/Users/bente/'
  alias psh=/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe
  alias cmd=/mnt/c/Windows/System32/cmd.exe
  alias adb=/mnt/c/Program Files/adb-fastboot/adb.exe
fi
