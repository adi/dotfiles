" Plugins in use
call plug#begin('~/.vim/plugged')

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'fatih/vim-go'

Plug 'iberianpig/tig-explorer.vim'

Plug 'psliwka/vim-smoothie'
Plug 'ap/vim-buftabline'
Plug 'junegunn/seoul256.vim'

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'adi/vim-indent-rainbow'
Plug 'adi/vim-left-decorators'

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
set number " classic line numbers should not be visible by default but this together with the next line make hybrid lines
set relativenumber " relative line numbers should be visible by default
set hlsearch " highlight search matches
set ignorecase " ignore case by default when searching
set incsearch " search as you type
set tabstop=4 " how large should a tab look
set shiftwidth=4 " how much should indent when going to the next line (if not a multiple of tabstop, it will add extra spaces)
set title " display file name in shell title
set laststatus=2 " display file name at bottom of window
set signcolumn=yes " always display the left gutter
set mouse=a " activate mouse support

" Syntax highlighting
filetype plugin on " activate file type detection
syntax on " activate syntax highlighting

" Color theme
let g:seoul256_background = 233 " darkest variant of seoul256 selected
colorscheme seoul256 " tell vim to use a theme
hi SignColumn ctermbg=232
hi LineNr ctermbg=232 ctermfg=237

" Cursor shapes for modes

let &t_SI = "\e[6 q"
let &t_SR = "\e[4 q" 
let &t_EI = "\e[2 q"
let &t_ER = "\e[2 q"

" Snapiness
set timeoutlen=1000 ttimeoutlen=50

" Extremely cool generic vim key mappings

nnoremap <silent> <S-Left> :bp<CR>
nnoremap <silent> <S-Right> :bn<CR>
nnoremap <silent> <S-Q> :bd<CR>

nnoremap <silent> <Tab> <C-W><C-W>

nnoremap ; :

" Specific termux stuff

if system("which termux-setup-storage")!= ""
	nnoremap <silent> <leader>v :r !termux-clipboard-get<CR>
    nnoremap <silent> <leader>c :.w !termux-clipboard-set<CR>
endif

" Folding options

set foldmethod=indent
augroup gofoldgroup
	au FileType go set foldmethod=syntax
augroup END
set foldlevelstart=20
nnoremap <space> za
function! NoBullshitFolding()
    let spaces = repeat(" ", &tabstop)
    let linetext = substitute(getline(v:foldstart), "\t", spaces, "g")
    return linetext
endfunction
set foldtext=NoBullshitFolding()

" file browsing settings

nnoremap <leader>e :Explore<CR>

let g:netrw_banner=0 " don't display annoying banner

" fatih/vim-go settings

let g:go_code_completion_enabled = 0 " let coc-go do this
let g:go_doc_keywordprg_enabled = 0 " let coc-go do this

let g:go_diagnostics_level = 2 " find compiling issues while editing
let g:go_highlight_functions = 1
let g:go_highlight_function_calls = 1
let g:go_highlight_variable_assignments = 1
let g:go_highlight_variable_declarations = 1
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_generate_tags = 1
let g:go_def_mode = 'guru'

" jreybert/vimagit settings

let g:magit_discard_untracked_do_delete = 1

" junegunn/fzf.vim settings

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

