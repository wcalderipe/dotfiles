" plugin system block
call plug#begin('~/.local/share/nvim/plugged')

" theme
Plug 'morhetz/gruvbox'

" javascript
Plug 'pangloss/vim-javascript'
Plug 'maxmellon/vim-jsx-pretty', { 'for': ['javascript'] }

" less and css syntax highlight
Plug 'groenewege/vim-less'

" async runner 
Plug 'neomake/neomake'

" async autocomplete
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" fuzzy file finder
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

" word finder
Plug 'mileszs/ack.vim'

" editor enchanters
Plug 'airblade/vim-gitgutter'
Plug 'vim-airline/vim-airline'
Plug 'scrooloose/nerdtree'
Plug 'editorconfig/editorconfig-vim'

" initialize plugin system
call plug#end()

" load initializers *.vim
runtime! initializers/*.vim

filetype off

" theme
syntax enable
colorscheme gruvbox
set background=dark

" line numbers
set nu

" 80 characters ruler
set cc=80

" do not break lines at end of the screen
set nowrap

" folding
set foldlevelstart=10
set foldmethod=syntax
