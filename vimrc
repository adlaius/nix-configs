" important!
set nocompatible

set autoindent
" make the TAB key expand into n spaces, where n is given by 'set tabstop=n'
set expandtab
" actual char width of tabstops
set tabstop=4
" char width of <, > shifting in Visual mode
set shiftwidth=4
" treat BS, DEL over spaces as if it were over tabs (i.e. delete n chars)
set softtabstop=4

" on some distros (current Debian & Fedora, for instance), /etc/vimrc gets
" sourced and sets filetype before we even get to our personal .vimrc;
" we therefore need to unset it, call pathogen, and only then enable it.
"filetype off
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

if has("autocmd")
  filetype plugin indent on
endif

syntax on
set background=dark

" allow user to switch to/from dirty buffers without forcing a write
set hidden

" by default, Y is 'yank (entire) line'; map it to 'yank to end of line' to
" give it consistency with C and D.
map Y y$

" N.B. the 'bubble shift' mappings rely on Tim Pope's unimpaired.vim plugin
" 'bubble' shift lines up/down
nmap <C-k> [e
nmap <C-j> ]e

" 'bubble' shift line blocks up/down
vmap <C-k> [egv
vmap <C-j> ]egv

" Show syntax highlighting groups for word under cursor
" Tip: http://stackoverflow.com/questions/1467438/find-out-to-which-highlight-group-a-particular-keyword-symbol-belongs-in-vim
nmap <C-S-P> :call <SID>SynStack()<CR>
function! <SID>SynStack()
    if !exists("*synstack")
        return
    endif
    echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

