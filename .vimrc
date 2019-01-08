" Initilize Plug
call plug#begin('~/.vim/plugged')

" Theme
Plug 'chriskempson/base16-vim'

" Lint engine
Plug 'w0rp/ale'

" JavaScript
Plug 'pangloss/vim-javascript'
Plug 'maxmellon/vim-jsx-pretty', { 'for': ['javascript'] }

" TypeScript syntax highlight and omni-completion
Plug 'leafgarland/typescript-vim'
Plug 'Quramy/tsuquyomi'

" LESS and CSS syntax highlight
Plug 'groenewege/vim-less'

" Fuzzy file search
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

" Word search
Plug 'mileszs/ack.vim'

" Detect and strip trailing whitespace
Plug 'ntpeters/vim-better-whitespace'

" Editor enchanters
Plug 'airblade/vim-gitgutter'
Plug 'scrooloose/nerdtree'
Plug 'editorconfig/editorconfig-vim'

" Start Plug system
call plug#end()

" Load plugins custom configurations from ~/.vim/initializers
runtime! initializers/*.vim

" Disable file type detection
filetype off

" Colors
syntax enable
colors zenburn
set background=dark

" Show line numbers
set nu

" Show 80 and 120 characters ruler
set cc=80,120

" Don't break lines at end of screen
set nowrap

" Code folding
set foldlevelstart=10
set foldmethod=syntax

" Disable status bar line colors
set statusline=%<%.80f\ %h%w%m%r%y%=%-12(\ %l,%c\ %)%P

" Enable backspace in insert mode
set backspace=2

" Don't write backup files
set nowritebackup
set noswapfile
set nobackup

" Add custom command to pretty-print JSON
com! FormatJSON %!json_pp
