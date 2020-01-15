" Leader commands
" Inspired by Doom Emacs
let mapleader = "\<Space>"
nmap <Leader>b :Buffers<CR>
" \d to insert date header (used in notes)
nmap <Leader>d "=strftime('### %Y-%m-%dT%H:%M')<CR>p
nmap <Leader>ff :Files<CR>
nmap <Leader>fh :History<CR>
nmap <Leader>g :Git<CR>
nmap <Leader>h :Helptags!<CR>
nmap <Leader>l :BLines<CR>
nmap <Leader>m :Marks<CR>
nmap <Leader>s :Filetypes<CR>
nmap <Leader>w <C-W>
" \z to kill the current buffer and switch to the last use one
nmap <Leader>z :bprevious\|bdelete! #<CR>
nmap <Leader><Space> :GFiles<CR>
nmap <Leader>: :History:<CR>

" coc
" Mostly copied from https://github.com/neoclide/coc.nvim/blob/b59c1dffb48370c342b63106a0d47970e5347b42/Readme.md

" https://github.com/neoclide/coc.nvim/issues/9#issuecomment-433638408
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-y>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ "\<C-x>\<C-e>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

inoremap <silent><expr> <c-space> coc#refresh()
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

autocmd CursorHold * silent call CocActionAsync('highlight')

nmap <leader>rn <Plug>(coc-rename)
xmap <leader>t  <Plug>(coc-format-selected)
nmap <leader>t  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>ac  <Plug>(coc-codeaction)
nmap <leader>qf  <Plug>(coc-fix-current)
nmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <S-TAB> <Plug>(coc-range-select-backword)

command! -nargs=0 Format :call CocAction('format')
command! -nargs=? Fold :call     CocAction('fold', <f-args>)
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

"nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
"nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
"nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
"nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
"nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
"nnoremap <silent> <space>j  :<C-u>CocNext<CR>
"nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
"nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
