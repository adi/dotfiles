" Plugins in use
call plug#begin('~/.vim/plugged')

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'fatih/vim-go'

Plug 'tpope/vim-fugitive'
Plug 'jreybert/vimagit'

Plug 'junegunn/seoul256.vim'

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

call plug#end()

" Generic vim common sense settings

set nocompatible " we don't need vi compat
set backspace=indent,eol,start " make backspace behave normally
set wildmenu " display command completion alternatives
set hidden " allow unsaved buffer to be hidden
set completeopt-=preview " doesn't show a new window with the definition of the completed item
set noswapfile " prevents certain build systems from freking out when seeing .swp files
set nobackup " prevents backup
set nowritebackup " prevents backup
set nonumber " classic line numbers should not be visible by default
set hlsearch " highlight search matches
set ignorecase " ignore case by default when searching
set incsearch " search as you type
set tabstop=4 " how large should a tab look
set shiftwidth=4 " how much should indent when going to the next line (if not a multiple of tabstop, it will add extra spaces)
set title " display file name in shell title

filetype plugin on " activate file type detection
syntax on " activate syntax highlighting
let g:seoul256_background = 233 " darkest variant of seoul256 selected
colorscheme seoul256 " tell vim to use a theme

" Extremely cool generic vim key mappings

nnoremap <leader>e :Explore<CR>

nnoremap <silent> <S-Left> :bp<CR>
nnoremap <silent> <S-Right> :bn<CR>
nnoremap <silent> <Tab> <C-W><C-W>
nnoremap ; :

" Folding options

set foldmethod=syntax
set foldlevelstart=20
nnoremap <space> za
function! NoBullshitFolding()
    let spaces = repeat(" ", &tabstop)
    let linetext = substitute(getline(v:foldstart), "\t", spaces, "g")
    return linetext
endfunction
set foldtext=NoBullshitFolding()

" fatih/vim-go settings

let g:go_code_completion_enabled = 0 " let coc-go do this
let g:go_doc_keywordprg_enabled = 0 " let coc-go do this

let g:go_diagnostics_level = 2 " find compiling issues while editing
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_variable_assignments = 1
let g:go_highlight_variable_declarations = 1
let g:go_highlight_types = 1

" jreybert/vimagit settings

let g:magit_discard_untracked_do_delete = 1

" junegunn/fzf.vim settings

nnoremap <silent><leader>r :Rg<CR>
nnoremap <silent>// :Rg <C-R>=expand("<cword>")<CR><CR>
vnoremap <silent>// y:Rg <C-R>"<CR>

nnoremap <silent><leader>g :GFiles<CR>
nnoremap <silent><leader>f :Files<CR>
nnoremap <silent><leader>b :Buffers<CR>
nnoremap <silent><leader>h :History<CR>
nnoremap <silent><leader>t :Tags<CR>

" neoclide/coc.nvim settings

autocmd BufWritePre *.go :silent call CocAction('runCommand', 'editor.action.organizeImport')

let g:coc_global_extensions = ['coc-go', 'coc-phpls', 'coc-python', 'coc-tsserver', 'coc-json', 'coc-git']

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-@> coc#refresh()

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

