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
set wrap linebreak breakindent
set hidden 
set nobackup nowritebackup
set cmdheight=2
set updatetime=300
" Don't give 'no match' messages
set shortmess+=c
" Always show the column on the left for error messages
set signcolumn=yes

let g:auto_save = 1

hi StatusLine cterm=bold ctermfg=11
hi StatusLineNC cterm=bold ctermfg=13
hi VertSplit cterm=NONE ctermfg=2
hi SignColumn ctermbg=0

au FileType html,markdown,text setl spell
