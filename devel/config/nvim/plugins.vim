call plug#begin('~/.config/nvim/plugged')

" Sensible defaults
Plug 'tpope/vim-sensible'

" Pretty
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'ap/vim-css-color'
Plug 'junegunn/goyo.vim'

" Because I'm lazy
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'
Plug 'ntpeters/vim-better-whitespace'
Plug 'dkarter/bullets.vim'
Plug 'dhruvasagar/vim-table-mode'

Plug '907th/vim-auto-save'
let g:auto_save = 1

" Integration
Plug 'xuhdev/vim-latex-live-preview'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'neoclide/coc.nvim', {'tag': '*', 'do': { -> coc#util#install()}}
Plug 'mattn/emmet-vim'
Plug 'jpalardy/vim-slime'
Plug '~/src/coc-mb'

call plug#end()

let g:coc_global_extensions = [
      \ 'coc-git',
      \ 'coc-lists',
      \
      \ 'coc-css',
      \ 'coc-json',
      \ 'coc-python',
      \ 'coc-rls',
      \ 'coc-tsserver',
      \
      \ 'coc-eslint',
      \ 'coc-tslint',
      \ 'coc-prettier',
      \ ]

let g:slime_target='neovim'
