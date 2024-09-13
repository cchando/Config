""""""""""""""""""""""""""""""""""""""""""
" Enable 24-bit true color display
if has("termguicolors")  " set true colors
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
endif


if has("autocmd")
  " Open file at last edited position
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
  " Prevent interferences with history items and jumping to errors in quickfix
  " list
  autocmd CmdwinEnter * nnoremap <CR> <CR>
  autocmd BufReadPost quickfix nnoremap <CR> <CR>

  " Set purescript files to open with haskell syntax 
  ""autocmd BufNewFile,BufRead *.purs set syntax=haskell

  " Set haskell files to open with purescript syntax 
  au BufRead,BufNewFile *.hs set filetype=purescript
endif


"colorscheme night-owl 
colorscheme onehalfdark


" ------- Plugins -----------------
" After editing, either restart vim (or run :source %), then run :PlugInstall 
call plug#begin('~/.vim/plugged')

Plug 'neovimhaskell/haskell-vim'
Plug 'raichoo/purescript-vim'

call plug#end()
" ----------------------------------




set nocompatible            " Disable vi compatibility
set mouse=
set ttymouse=
"" set mouse=a              " Enable the mouse
set showcmd                 " show the current command including leader key 
"set relativenumber         " Relative line numbers
filetype on                 " filetype must be 'on' before setting it 'off'
syntax on                   " Syntax highlighting
filetype indent on          " enable detection and indent

set encoding=utf-8 nobomb   " BOM often causes trouble, UTF-8 is awsum.

set hidden                  " can put buffer to the background without writing
                            "   to disk, will remember history/marks.
set lazyredraw              " don't update the display while executing macros
set ttyfast                 " Send more characters at a given time.

" --- search / regexp ---
set gdefault                " RegExp global by default
set magic                   " Enable extended regexes.
set hlsearch                " highlight searches
set incsearch               " show the `best match so far' astyped
set ignorecase smartcase    " make searches case-insensitive, unless they
                            "   contain upper-case letters

" --- keys ---
set backspace=indent,eol,start  " allow backspacing over everything.
set esckeys                     " Allow cursor keys in insert mode.
set nostartofline               " Make j/k respect the columns
" set virtualedit=all           " allow the cursor to go in to 'invalid' places
set timeoutlen=300  "500        " how long it waits for mapped commands
set ttimeoutlen=100             " faster timeout for escape key and others

" -----------------------------------------------------------------------------
" UI
" -----------------------------------------------------------------------------

set statusline=0          " Disable status line
""set laststatus=2          " Always show status line
""set showtabline=2         " Always show tab line (top of screen)
set tabline=
set tabline+=\ %f
" default the tabline to green when entering Vim
hi tabline guifg=#d6deeb ctermfg=253 guibg=#011627 ctermbg=233 gui=NONE cterm=NONE
""set cursorline              " Highlight current line
set number                  " Enable line numbers.
set numberwidth=9        " width of numbers line (default on gvim is 4)
set report=0                " Show all changes.
set showmode                " Show the current mode.
set showcmd                 " show partial command on last line of screen.
set showmatch               " show matching parenthesis
set splitbelow splitright   " how to split new windows.

set scrolloff=5             " Start scrolling n lines before horizontal
                            "   border of window.
set sidescrolloff=7         " Start scrolling n chars before end of screen.
set sidescroll=1            " The minimal number of columns to scroll
                            "   horizontally.

" --- command completion ---
set wildmenu                " Hitting TAB in command mode will
set wildchar=<TAB>          "   show possible completions.
set wildmode=list:longest

" --- diff ---
set diffopt=filler          " Add vertical spaces to keep right
                            "   and left aligned.
set diffopt+=iwhite         " Ignore whitespace changes.

" --- folding---
set foldmethod=manual       " manual fold
set foldnestmax=3           " deepest fold is 3 levels
set nofoldenable            " don't fold by default

" --- list chars ---
" list spaces and tabs to avoid trailing spaces and mixed indentation
" see key mapping at the end of file to toggle `list`
"" set listchars=tab:▹\ ,trail:·,nbsp:⚋
"" set fillchars=fold:-
"" set list

" --- remove sounds effects ---
set noerrorbells
""set visualbell

" -----------------------------------------------------------------------------
" => Files, backups and undo
" -----------------------------------------------------------------------------
" Turn backup off, since most stuff is in SVN, git, etc anyway...
set nobackup
set nowb
set noswapfile
set undolevels=1000      " use many muchos levels of undo
set title                " change the terminal's title
            
" -----------------------------------------------------------------------------
" INDENTATION AND TEXT-WRAP
" -----------------------------------------------------------------------------

set expandtab                   " Expand tabs to spaces
set autoindent smartindent      " auto/smart indent
set copyindent                  " copy previous indentation on auto indent
set softtabstop=4               " Tab key results in # spaces
set tabstop=4                   " Tab is # spaces
set shiftwidth=4                " The # of spaces for indenting.
set smarttab                    " At start of line, <Tab> inserts shift width
                                "   spaces, <Bs> deletes shift width spaces.

set wrap                        " wrap lines
set textwidth=80
"set colorcolumn=+1              " Show large lines
set formatoptions=qrn1          " automatic formating.
set formatoptions-=o            " don't start new lines w/ comment leader on
                                "   pressing 'o'

set nomodeline                  " don't use modeline (security)

:let mapleader = " " 
set pastetoggle=<C-a>       " paste mode: avoid auto indent, treat chars
                                "   as literal.

noremap R <C-r>
noremap , m
nnoremap f :call search('\<' . nr2char(getchar()), 'W', line('.'))<cr>
" Split line (dual to [J]oin lines)
" The normal use of S is covered by cc, so don't worry about shadowing
nnoremap S i<cr><esc><right>
" Clear the search highlight
nnoremap <silent> \ :silent nohlsearch<CR>
noremap J 10j
noremap K 10k
noremap ga 0
noremap gl $
noremap gh ^
noremap gm %
noremap gi $i<right>
noremap <C-e> :q!<CR>
noremap <C-s> :split<CR>
inoremap fd <Esc>l
nnoremap gr :w<CR>
nnoremap mm zz
nnoremap mt zt
nnoremap mb zb
nnoremap zz <Nop>
nnoremap zt <Nop>
nnoremap zb <Nop>
inoremap <C-w> <C-\><C-o>:w<CR>
noremap <C-j> <C-d>
noremap <C-k> <C-u>
noremap <C-u> <C-b>
noremap <C-d> <C-f>
inoremap <C-z> <Nop>
noremap <C-s> <Nop>
inoremap <C-s> <Nop>
noremap <Up>    <Nop>
noremap <Down>  <Nop>
noremap <Left>  <Nop>
noremap <Right> <Nop>
inoremap <Up>    <Nop>
inoremap <Down>  <Nop>
inoremap <Left>  <Nop>
inoremap <Right> <Nop>
noremap <C-p> ciw<C-r>0<ESC>b
" let loaded_matchparen = 1
highlight MatchParen ctermbg=blue guibg=lightblue
