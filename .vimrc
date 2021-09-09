let g:go_fmt_command = "goimports"

set completeopt-=preview
set noswapfile
set number
set hlsearch
set ignorecase
set incsearch
set tabstop=4
set shiftwidth=4

nnoremap <C-Left> :tabprevious<CR>
nnoremap <C-Right> :tabnext<CR>

colorscheme codedark

let g:airline_theme = 'codedark'

let g:go_diagnostics_level = 2
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_variable_assignments = 1
let g:go_highlight_variable_declarations = 1
let g:go_highlight_types = 1

let g:magit_discard_untracked_do_delete = 1

