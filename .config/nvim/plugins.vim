let plugpath = expand('<sfile>:p:h'). '/autoload/plug.vim'
if !filereadable(plugpath)
    if executable('curl')
        let plugurl = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
        call system('curl -fLo ' . shellescape(plugpath) . ' --create-dirs ' . plugurl)
        if v:shell_error
            echom "Error downloading vim-plug. Please install it manually.\n"
            exit
        endif
    else
        echom "vim-plug not installed. Please install it manually or install curl.\n"
        exit
    endif
endif

call plug#begin('~/.config/nvim/plugged')

"Plug 'ntpeters/vim-better-whitespace'
"Plug 'rstacruz/vim-closer'
"Plug 'easymotion/vim-easymotion'
Plug 'neoclide/coc.nvim', {'tag': '*', 'do': { -> coc#util#install()}}
Plug 'Shougo/denite.nvim'
Plug 'tpope/vim-sensible'
"Plug 'tpope/vim-fugitive'
"Plug 'tpope/vim-surround'
"Plug 'scrooloose/syntastic'
Plug 'tpope/vim-markdown'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
Plug 'mattn/emmet-vim'
Plug 'neovimhaskell/haskell-vim'
Plug '907th/vim-auto-save'
Plug 'itchyny/vim-haskell-indent'

call plug#end()
