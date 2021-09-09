filetype plugin on
set omnifunc=syntaxcomplete#Complete
syntax on

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

" air-line
let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = ' '
let g:airline_symbols.colnr = '✕'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'
let g:airline_symbols.readonly = 'r/o'
let g:airline_symbols.maxlinenr = ''

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'

let g:go_diagnostics_level = 2
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_variable_assignments = 1
let g:go_highlight_variable_declarations = 1
let g:go_highlight_types = 1

let g:magit_discard_untracked_do_delete = 1
let g:netrw_localrmdir='rm -r'

command -nargs=1 Sch noautocmd vimgrep /<args>/gj `git ls-files` | cw

