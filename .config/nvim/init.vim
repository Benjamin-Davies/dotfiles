source ~/.config/nvim/plugins.vim
source ~/.config/nvim/keymaps.vim

filetype plugin indent on

" Indent with 2 spaces
set tabstop=2
set shiftwidth=2
set expandtab

" Self explanatory
set number relativenumber
set splitbelow splitright

let g:auto_save = 1

hi StatusLine cterm=bold ctermfg=11
hi StatusLineNC cterm=bold ctermfg=13
