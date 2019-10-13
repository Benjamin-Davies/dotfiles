call plug#begin('~/.config/nvim/plugged')

Plug 'junegunn/fzf.vim'
"Plug 'ntpeters/vim-better-whitespace'
"Plug 'rstacruz/vim-closer'
"Plug 'easymotion/vim-easymotion'
Plug 'neoclide/coc.nvim', {'tag': '*', 'do': { -> coc#util#install()}}
Plug 'Shougo/denite.nvim'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
"Plug 'scrooloose/syntastic'
Plug 'tpope/vim-markdown'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
Plug 'mattn/emmet-vim'
Plug 'neovimhaskell/haskell-vim'
Plug '907th/vim-auto-save'
Plug 'itchyny/vim-haskell-indent'
Plug 'lervag/vimtex'

call plug#end()
