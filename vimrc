" designed for vim 8+
let skip_defaults_vim=1
set nocompatible

" ----------------- Vi Compatible (~/.exrc)-----------------------------

" automatically indent new lines
set autoindent

" automatically write files when changing when multiple files open
set autowrite

" activate line numbers
set number

" turn col and row position on in bottom right
set ruler " see ruf for formatting

" show command and insert mode
set showmode

set tabstop=2

" ----------------- Misc Stuff -----------------------------------------

" to support german Umlaute
set encoding=utf-8
scriptencoding utf-8

" max line length = 0, so now auto newline chars
set tw=0

" Prevent automatic newline characters in txt files on windows
autocmd BufRead,BufNewFile   *.txt setlocal formatoptions-=t formatoptions+=croql

" No beeps
set noerrorbells

" Being able to copy to system clipboard (had problems before)
set mouse=v

" This will enable the popup menu for usage of spell check
set mousemodel=popup

" ----------------------------------------------------------------------

set softtabstop=2

" mostly used with >> and <<
set shiftwidth=2

set smartindent

set smarttab

if v:version >= 800
  " stop vim from silently messing with files that it shouldn't
  set nofixendofline

  " better ascii friendly listchars
  set listchars=space:*,trail:*,nbsp:*,extends:>,precedes:<,tab:\|>

  " i fucking hate automatic folding
  set foldmethod=manual
  set nofoldenable
endif

" mark trailing spaces as errors
match ErrorMsg '\s\+$'

" replace tabs with spaces automatically
set expandtab

" prevents truncated yanks, deletes, etc (important!)
" increases the maximum buffer size so temp data will not be lost
set viminfo='20,<1000,s1000

" Makes backspace key more powerful. Required for mac.delete to work.
set backspace=indent,eol,start

" command history
set history=100

" Show me what I'm typing
set showcmd

" Here because plugins and stuff need it
syntax enable

" faster scrolling
set ttyfast

" allow sensing the filetype
filetype plugin on

" set the background to dark
set bg=dark

" prevent the cursor from moving off screen
set scrolloff=2

" keep the terminal title updated
set laststatus=0
set icon

" not always a fan of bracket matching or folding
let g:loaded_matchparen=1
set noshowmatch

" Automatically save before :next, :make etc.
set autowrite

" Automatically reread changed files without asking me anything
set autoread

" Time out on key codes but not mappings.
" Basically this makes terminal Vim work sanely.
set notimeout
set ttimeout
set ttimeoutlen=10

" ----------------- Searching ------------------------------------------

" wrap around when searching
set wrapscan

" search highlights
set hlsearch

" Shows the match while typing
set incsearch

" Search case insensitive...
set ignorecase

" ... but not when search pattern contains upper case characters
set smartcase

" ----------------- Some Settings taken from jessfraz ------------------

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" If linux then set ttymouse
let s:uname = system("echo -n \"$(uname)\"")
if !v:shell_error && s:uname == "Linux" && !has('nvim')
  set ttymouse=xterm
endif

" ----------------- Python Files ---------------------------------------
" https://realpython.com/vim-and-python-a-match-made-in-heaven/
au BufNewFile,BufRead *.py  " Upon loading *.py files.
    \ set tabstop=4 |
    \ set softtabstop=4 |
    \ set shiftwidth=4 |
    \ set expandtab |
    \ set autoindent |
    \ set fileformat=unix

" also folding from the same article
set foldmethod=indent
set foldlevel=99

" ----------------- Backup Stuff ---------------------------------------

" configure backup to one central folder
" Check if the .vimtmp folder exists in $HOME. If not it will be created
if !isdirectory($HOME."/.vimtmp")
    call mkdir($HOME."/.vimtmp", "", 0770)
endif

set backupdir=$HOME/.vimtmp//,. " for backup files
set directory=$HOME/.vimtmp//,. " for swap files
set undodir=$HOME/.vimtmp//,. " for undo files
" The double slash at the end ensures that there is no conflict in case of two files
" having the same name (maybe only honored for swap files)
" The ,. allow vim to use the current directory if the former doesn't exist.

set undofile " enable undo files
set backup " enable backup (and swap)

" ----------------- Plugins --------------------------------------------

" Install vim-plug if not already installed. According to rwxrob plug
" is better and Vim 8 Plugins suck
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
      \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
  echo "Don't forget to GoInstallBinaries if you're doing Go dev."
endif

" only load plugins if Plug detected
if filereadable(expand("~/.vim/autoload/plug.vim"))
  call plug#begin('~/.vimplugins')
  Plug 'vim-pandoc/vim-pandoc'
  Plug 'vim-pandoc/vim-pandoc-syntax'

  " Taken from: https://www.vimfromscratch.com/articles/vim-for-python/
  Plug 'tpope/vim-commentary'
  Plug 'jeetsukumaran/vim-pythonsense'
  Plug 'jiangmiao/auto-pairs'
  Plug 'sheerun/vim-polyglot'

  Plug 'davidhalter/jedi-vim' " the jedi completion engine for Python
  Plug 'lifepillar/vim-mucomplete' " Minimalist autocomplete popup
  Plug 'morhetz/gruvbox'
  Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }
  call plug#end()

  " golang
  let g:go_fmt_fail_silently = 0
  let g:go_fmt_command = 'goimports'
  let g:go_fmt_autosave = 1
  let g:go_gopls_enabled = 1
  let g:go_highlight_types = 1
  let g:go_highlight_fields = 1
  let g:go_highlight_functions = 1
  let g:go_highlight_function_calls = 1
  let g:go_highlight_operators = 1
  let g:go_highlight_extra_types = 1
  let g:go_highlight_variable_declarations = 1
  let g:go_highlight_variable_assignments = 1
  let g:go_highlight_build_constraints = 1
  let g:go_highlight_diagnostic_errors = 1
  let g:go_highlight_diagnostic_warnings = 1
  "let g:go_auto_type_info = 1 " forces 'Press ENTER' too much
  let g:go_auto_sameids = 0
  "let g:go_metalinter_command='golangci-lint'
  "let g:go_metalinter_command='golint'
  "let g:go_metalinter_autosave=1
  set updatetime=100
  "let g:go_gopls_analyses = { 'composites' : v:false }
  au FileType go nmap <leader>t :GoTest!<CR>
  au FileType go nmap <leader>v :GoVet!<CR>
  au FileType go nmap <leader>b :GoBuild!<CR>
  au FileType go nmap <leader>c :GoCoverageToggle<CR>
  au FileType go nmap <leader>i :GoInfo<CR>
  au FileType go nmap <leader>l :GoMetaLinter!<CR>
else
  autocmd vimleavepre *.go !gofmt -w % " backup if fatih fails
endif

" colorscheme gruvbox8 " set the colorscheme (now installed)
colorscheme gruvbox

" enable omni-completion, needs filetype plugin on
set omnifunc=syntaxcomplete#Complete

" Make the Python code look pretty
let python_highlight_all=1
syntax on

" Settings for vim-jedi (Python)
"let g:jedi#goto_command = "<leader>d"
"let g:jedi#goto_assignments_command = "<leader>g"
"let g:jedi#goto_stubs_command = "<leader>s"
"let g:jedi#goto_definitions_command = ""
"let g:jedi#documentation_command = "K"
"let g:jedi#usages_command = "<leader>n"
"let g:jedi#completions_command = "<C-Space>"
"let g:jedi#rename_command = "<leader>r"

" Settings for the mucomplete plugin
set completeopt+=menuone
set completeopt+=noinsert
set shortmess+=c  " Shut off completion messages
set belloff+=ctrlg " If Vim beebs during completion
let g:mucomplete#enable_auto_at_startup = 1  "autostart
let g:mucomplete#completion_delay = 1

" ----------------------------------------------------------------------

" From the defaul vimrc example file. When started as "evim", evim.vim
" evim.vim will already have done these settings, bail out.
if v:progname =~? "evim"
  finish
endif

" The matchit plugin makes the % command work better, but is not backwards
" compatible. The ! means the package will only load during plugin init.
if has('syntax') && has('eval')
  packadd! matchit
endif

