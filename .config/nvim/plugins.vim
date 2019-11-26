call plug#begin('~/.config/nvim/plugged')

" Sensible defaults
Plug 'tpope/vim-sensible'

" Because I'm lazy
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'ntpeters/vim-better-whitespace'
Plug 'dkarter/bullets.vim'
Plug 'dhruvasagar/vim-table-mode'

Plug 'junegunn/fzf.vim'
let uname = substitute(system('uname'), '\n', '', '')
if uname == 'Darwin'
  set runtimepath+=/usr/local/opt/fzf
endif

Plug '907th/vim-auto-save'
let g:auto_save = 1

" Integration
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-gitgutter'
Plug 'neoclide/coc.nvim', {'tag': '*', 'do': { -> coc#util#install()}}
Plug 'mattn/emmet-vim'

call plug#end()

let g:coc_global_extensions = [ 'coc-python', 'coc-rls', 'coc-json', 'coc-tsserver', 'coc-eslint', 'coc-prettier', 'coc-lists', 'coc-git' ]
