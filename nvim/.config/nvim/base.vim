syntax enable
set smartcase

filetype plugin indent on
set tabstop=4
set shiftwidth=4
set expandtab

colorscheme nord

let g:airline_theme='nord'
let g:airline_powerline_fonts = 1

set relativenumber

let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
let g:vimtex_compiler_method='latexmk'
" set conceallevel=1
" let g:tex_conceal='abdmg'

" autocmd! UltiSnips_AutoTrigger 0
set runtimepath+=~/.config/nvim/my_snippets
let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'
let g:UltiSnipsSnippetDirectories=["UltiSnips", "my_snippets"]
