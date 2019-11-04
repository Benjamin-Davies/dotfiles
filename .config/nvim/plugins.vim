call plug#begin('~/.config/nvim/plugged')

" Sensible defaults
Plug 'tpope/vim-sensible'

" Because I'm lazy
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'ntpeters/vim-better-whitespace'
Plug 'dkarter/bullets.vim'

Plug 'junegunn/fzf.vim'
let uname = substitute(system('uname'), '\n', '', '')
if uname == 'Darwin'
  set runtimepath+=/usr/local/opt/fzf
endif

Plug '907th/vim-auto-save'
let g:auto_save = 1

" Highlighting
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
Plug 'tpope/vim-markdown'
Plug 'lervag/vimtex'
Plug 'cespare/vim-toml'

" Tool integration
Plug 'tpope/vim-fugitive'
Plug 'neoclide/coc.nvim', {'tag': '*', 'do': { -> coc#util#install()}}
Plug 'mattn/emmet-vim'

call plug#end()
