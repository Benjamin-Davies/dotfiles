hi StatusLine cterm=bold ctermfg=11
hi StatusLineNC cterm=bold ctermfg=13

hi VertSplit cterm=NONE ctermfg=2
hi SignColumn ctermbg=NONE

hi Title ctermfg=4
hi SpellBad cterm=underline ctermbg=NONE
hi SpellBad cterm=underline ctermbg=NONE
hi SpellBad cterm=underline ctermbg=NONE

let $FZF_DEFAULT_OPTS = '--color fg:-1,bg:-1,hl:4,fg+:-1,bg+:-1,hl+:2'
autocmd! User FzfStatusLine setlocal statusline=
let g:colors_name = 'my-light'
