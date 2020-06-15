source ~/.config/nvim/plugins.vim
source ~/.config/nvim/keymaps.vim

colorscheme my-light

" Allow filetype detection
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

let g:tex_flavor = "latex"
au FileType html,markdown,text,tex setl spell
