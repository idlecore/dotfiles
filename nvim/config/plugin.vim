
call plug#begin('$SOURCE_PATH/plugins')
Plug 'scrooloose/nerdtree'
Plug 'raimondi/delimitmate'
Plug 'iCyMind/NeoSolarized'
Plug 'elmcast/elm-vim'
Plug 'neovimhaskell/haskell-vim'
Plug 'sickill/vim-monokai'
Plug 'liuchengxu/graphviz.vim'
Plug 'tpope/vim-fireplace'
Plug 'morhetz/gruvbox'
call plug#end()


""""""""""""""""""""
" NERDTree
""""""""""""""""""""
nnoremap <silent> <Leader>n :NERDTreeToggle<CR>

""""""""""""""""""""
" Graphviz
""""""""""""""""""""
let g:graphviz_output_format = "png"
nnoremap <silent> <Leader>gg :Graphviz!<CR>
nnoremap <silent> <Leader>gc :GraphvizCompile<CR>
