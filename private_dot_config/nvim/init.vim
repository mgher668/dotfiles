" MGHER's neovim config

" General Settings

if (has("termguicolors"))
 set termguicolors
endif

source ~/.config/nvim/plugins.vim

set encoding=UTF-8

" set background=dark
" set background=light

" keep cursor horizontal position while scrolling
" set virtualedit=all

" set Vim-specific sequences for RGB colors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

" The configuration options should be placed before `colorscheme sonokai`.
" let g:sonokai_style = 'andromeda'
let g:sonokai_style = 'atlantis'
let g:sonokai_better_performance = 1
let g:sonokai_cursor = 'blue'
let g:sonokai_menu_selection_background = 'green'
let g:edge_style = 'aura'
" let g:sonokai_transparent_background = 1

" colorscheme PaperColor
" colorscheme sonokai
" colorscheme terafox
" colorscheme catppuccin
colorscheme nightfox
" colorscheme carbonfox
" colorscheme neon
" let g:lightline.colorscheme = 'andromeda'
" colorscheme nightfly
" colorscheme rose-pine
" colorscheme tokyonight-moon
" colorscheme edge
" colorscheme duskfox
" colorscheme jellybeans
" colorscheme onedark
" colorscheme gruvbox
" colorscheme pencil
" colorscheme challenger_deep
" colorscheme falcon
" colorscheme github_dark
" let ayucolor="mirage"
" colorscheme ayu
" Set vim theme but also keeping the terminal transparent background
" highlight Normal     ctermbg=NONE guibg=NONE
" highlight LineNr     ctermbg=NONE guibg=NONE
" highlight SignColumn ctermbg=NONE guibg=NONE
" lua << EOF
" vim.g.neon_style = "dark"
" vim.g.neon_italic_keyword = true
" vim.g.neon_italic_function = true
" vim.g.neon_transparent = false
" vim.g.neon_italic_boolean = true
" vim.g.neon_bold = true

" vim.cmd[[colorscheme neon]]
" EOF

" highlight Normal guibg=NONE

" hi Search guibg=#FFCB8B guifg=#19647E

filetype plugin on
set omnifunc=syntaxcomplete#Complete

set cursorline

" set noshowmode

set tabstop=2 softtabstop=2
set shiftwidth=2
set expandtab
" set smartindent
" set ts=4 sw=4
set nowrap
set noswapfile
set nobackup
set undodir=~/.config/nvim/undodir
set undofile
set incsearch
set scrolloff=6
set signcolumn=auto
set colorcolumn=80
" set hlsearch
set nohlsearch
set shortmess-=S
" set shortmess+=T
" set shortmess=a

" set updatetime=400
" set updatetime=50
" set timeoutlen=1000
" set updatetime=200
set updatetime=100
" set timeoutlen=500
" set ttimeoutlen=50

" toggle paste mode
" set pastetoggle=<F3>
noremap <F4> :set hlsearch! hlsearch?<CR>
" noremap <silent> <C-p> :Files<CR>
noremap <silent> <C-g> :GFiles<CR>
" noremap <silent> <C-b> :Buffers<CR>
" noremap <silent> <C-o> :Buffers<CR>
" noremap <C-f> :Rg!
" noremap <C-f> :Clap grep2
" noremap ,m :NERDTreeToggle<CR>
" noremap ,n :NERDTreeFind<CR>
" nnoremap <leader>fx :ALEFix<CR>

" nnoremap <SPACE> <Nop>
let g:mapleader=","
" let g:mapleader="<Space>"

" disable the ZZ map in vim
nnoremap Z <Nop>
nnoremap ZZ <Nop>

" Terminal-mode namespace for mappings
" tnoremap <Esc> <C-\><C-n>
tnoremap <expr> <C-R> '<C-\><C-N>"'.nr2char(getchar()).'pi'
tnoremap <A-h> <C-\><C-N><C-w>h
tnoremap <A-j> <C-\><C-N><C-w>j
tnoremap <A-k> <C-\><C-N><C-w>k
tnoremap <A-l> <C-\><C-N><C-w>l
inoremap <A-h> <C-\><C-N><C-w>h
inoremap <A-j> <C-\><C-N><C-w>j
inoremap <A-k> <C-\><C-N><C-w>k
inoremap <A-l> <C-\><C-N><C-w>l
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l

" if !exists('g:lasttab')
"   let g:lasttab = 1
" endif
" nmap <Leader>tl :exe "tabn ".g:lasttab<CR>
" au TabLeave * let g:lasttab = tabpagenr()

" make vim register sync with system clipboard
set clipboard=unnamed

set nu
set relativenumber

" Search down into subfolders
" Provides tab-completion for all file-related tasks
set path+=**

" Disable all matching files when we tab complete
" set wildmode=longest,list,full
set wildmenu

" Setup fold
let g:ts_foldexpr_max_file_size = 512 * 1024

function! s:smart_foldmethod() abort
  let l:fullpath = expand('%:p')
  let l:filesize = getfsize(l:fullpath)

  if l:filesize >= 0 && l:filesize <= g:ts_foldexpr_max_file_size
    setlocal foldmethod=expr
    setlocal foldexpr=nvim_treesitter#foldexpr()
  else
    setlocal foldmethod=manual
    setlocal foldexpr=0
  endif
endfunction

augroup SmartFoldMethod
  autocmd!
  autocmd BufReadPost,BufNewFile * call s:smart_foldmethod()
augroup END

" set foldexpr=nvim_treesitter#foldexpr()
" set foldnestmax=10
" set nofoldenable
set foldlevel=99

" autocmd BufReadPost,FileReadPost * normal zR

set laststatus=3
" let gitBranch=system("git rev-parse --abbrev-ref HEAD")
" " set statusline=%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P\ %=%{&filetype}\ %{gitBranch}\ %y\ %0(%{&ff},%{&fenc},%{&bomb})\ %0%{strftime(\"%c\")}
" set statusline=%F%m%r%h%w\ [POS=%04l,%04v]\ [%p%%]\ [LEN=%L]
" execute "set statusline +=" . gitBranch


" use mouse to nav
" set mouse=nvi
set mouse=a

" Netrw
" let g:netrw_banner = 0
let g:netrw_liststyle=3
" let g:netrw_browse_split = 4
" let g:netrw_winsize = 20
let g:netrw_silent=1

" nnoremap <silent> ze :set virtualedit=all<CR>
" nnoremap <silent> zE :set virtualedit=<CR>

" augroup AllowHorizontalScroll
"   autocmd!
"   autocmd CursorMoved * if col('.') <= len(getline('.')) | set virtualedit= | endif
" augroup END

" set spelling word
" set spell

"case sensitive search when using uppercase characters
set smartcase

" fater way to ESC from insert mode
" imap kj <ESC>

" open config file in a new tab
" nnoremap <leader>ev :tabedit $MYVIMRC<CR>

" reload config file
nnoremap <leader>sv :source $MYVIMRC<CR>

" navigate between buffers
" nnoremap <leader>j :bnext<CR>
" nnoremap <leader>k :bprevious<CR>
" nnoremap <leader>h :bfirst<CR>
" nnoremap <leader>l :blast<CR>
" nnoremap <leader>br :%bd|e<CR>

" show all buffers and let user choose one
nnoremap <leader>i :ls<CR>:b<Space>

" delete current buffer
" nnoremap <leader>dd :bd<CR>

" close curren window
" nnoremap <leader>c :q<CR>
nnoremap <leader>c :close<CR>
" Close the quickfix window.
nnoremap <leader>q :cclose<CR>

" write current buffer
nnoremap <leader>w :write<CR>

" navigate between tabs
nnoremap <leader>1 1gt
nnoremap <leader>2 2gt
nnoremap <leader>3 3gt
nnoremap <leader>4 4gt
nnoremap <leader>5 5gt
nnoremap <leader>6 6gt
nnoremap <leader>7 7gt
nnoremap <leader>8 8gt
nnoremap <leader>9 9gt
nnoremap <A-1> 1gt
nnoremap <A-2> 2gt
nnoremap <A-3> 3gt
nnoremap <A-4> 4gt
nnoremap <A-5> 5gt
nnoremap <A-6> 6gt
nnoremap <A-7> 7gt
nnoremap <A-8> 8gt
nnoremap <A-9> :tablast<CR>

" nnoremap <C-Tab> gt
" nnoremap <C-S-Tab> gT
nnoremap <C-A-k> :tabn<CR>
nnoremap <C-A-j> :tabp<CR>

" go to last tab
if !exists('g:lasttab')
  let g:lasttab = 1
endif
nmap <leader>tl :exe "tabn ".g:lasttab<CR>
au TabLeave * let g:lasttab = tabpagenr()

" toggle highlight current line
" nnoremap <leader>ar :setlocal cursorline!<CR>
nnoremap <leader>ar :set cursorline!<CR>

" toggle highlight current column
nnoremap <leader>ac :setlocal cursorcolumn!<CR>
" nnoremap <leader>ac :set cursorcolumn!<CR>

" switch between two buffer
" nnoremap <leader><tab> <C-^>

" noremap <leader>y "*y
" noremap <leader>p "*p
" noremap <leader>Y "+y
" noremap <leader>P "+p

xnoremap <leader>p "_dP

nnoremap <leader>y "+y
vnoremap <leader>y "+y
nmap <leader>Y "+Y

nnoremap <leader>d "_d
vnoremap <leader>d "_d

" cancelled
" inoremap <C-c> <Esc>

nnoremap <A-=> :res +1<CR>
nnoremap <A--> :res -1<CR>
nnoremap + :vertical resize +3<CR>
nnoremap _ :vertical resize -3<CR>

nnoremap <C-A-e> <C-e>j
nnoremap <C-A-y> <C-y>k
vnoremap <C-A-e> <C-e>j
vnoremap <C-A-y> <C-y>k

nnoremap n nzz
nnoremap N Nzz

" :nnoremap <silent> <c-Up> :resize -1<CR>
" :nnoremap <silent> <c-Down> :resize +1<CR>
" :nnoremap <silent> <c-left> :vertical resize -1<CR>
" :nnoremap <silent> <c-right> :vertical resize +1<CR>

" nnoremap <leader>bda :%bd|e#<CR>

" noremap <Leader>/ /<Ctrl-R>"
" :map <Leader>/ :let @/=@"<CR>

" inoremap kj <Esc>
" cnoremap kj <Esc>

" cnoremap <C-p> <C-r>"

" TODO: check this is helpful for big file or not
" it do work at multi-line but not single line
set synmaxcol=2048

" vertical split diff windows as default
set diffopt+=vertical

" spell languages
nnoremap <leader>sp :set spell!<CR>
set spelllang=en
set spellsuggest=best,9

" Press * to search for the term under the cursor or a visual selection and
" then press a key below to replace all instances of it in the current file.
nnoremap <leader>r :%s///g<Left><Left>
" Confirm at each one replace
nnoremap <leader>rc :%s///gc<Left><Left><Left>

nnoremap <C-l> <C-6>

" Move selection
nnoremap <A-S-j> :m .+1<CR>==
nnoremap <A-S-k> :m .-2<CR>==
" inoremap <A-j> <Esc>:m .+1<CR>==gi
" inoremap <A-k> <Esc>:m .-2<CR>==gi
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" Search down with case insensitive
nnoremap <leader>/ /\c
" Search up with case insensitive
nnoremap <leader>? ?\c

" Copy file path to system clipboard
nnoremap <leader>yp :let @+=expand('%:p')<CR>
nnoremap <leader>yr :let @+=expand('%')<CR>

nnoremap <leader>o :only<CR>

" nnoremap <C-M-Space> [{V%
" xnoremap <C-M-Space> "_y[{V%

set fileencodings=ucs-bom,utf-8,cp936,gb18030,big5,euc-jp,euc-kr,default,latin1
" set fileencodings=ucs-bom,utf-8,default,latin1

" auto switch fcitx5 input method
autocmd InsertLeave * :silent !fcitx5-remote -c
autocmd BufCreate * :silent !fcitx5-remote -c
autocmd BufEnter * :silent !fcitx5-remote -c
autocmd BufLeave * :silent !fcitx5-remote -c
