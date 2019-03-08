if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-sensible'
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-surround'
Plug 'scrooloose/syntastic'
Plug 'tpope/vim-markdown'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
Plug 'mattn/emmet-vim'
Plug 'neovimhaskell/haskell-vim'
Plug 'calebsmith/vim-lambdify'

call plug#end()

filetype plugin indent on

" Indent with 2 spaces
set tabstop=2
set shiftwidth=2
set expandtab

" Self explanatory
set number relativenumber
set splitbelow splitright

if exists('+colorcolumn')
  hi ColorColumn ctermbg=lightgrey
  set colorcolumn=80
endif

if exists(':tnoremap')
  tnoremap <Esc> <C-\><C-n>
endif
