" Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.config/nvim/autoload/plugged')

" Make sure you use single quotes

" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
Plug 'junegunn/vim-easy-align'

" Any valid git URL is allowed
" Plug 'https://github.com/junegunn/vim-github-dashboard.git'

" Multiple Plug commands can be written in a single line using | separators
" Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

" On-demand loading
" Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

" Using a non-default branch
" Plug 'rdnetto/YCM-Generator', { 'branch': 'stable' }

" Using a tagged release; wildcard allowed (requires git 1.9.2 or above)
" Plug 'fatih/vim-go', { 'tag': '*' }

" Plugin options
" Plug 'nsf/gocode', { 'tag': 'v.20150303', 'rtp': 'vim' }

" Plug '/usr/local/opt/fzf'
" Plugin outside ~/.vim/plugged with post-update hook
" Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Manage branches and tags with fzf.
Plug 'stsewd/fzf-checkout.vim'

" Plug 'jremmen/vim-ripgrep'
Plug 'mhinz/vim-grepper'

" ranger with vim
" Plug 'francoiscabrol/ranger.vim'

" vim-visual-multi
" Plug 'mg979/vim-visual-multi', {'branch': 'master'}

" lightline
" Plug 'itchyny/lightline.vim'

" A Git wrapper so awesome, it should be illegal
Plug 'tpope/vim-fugitive'
" GitHub extension for fugitive.vim
Plug 'tpope/vim-rhubarb'

" A git blame plugin for neovim inspired by VS Code's GitLens plugin
Plug 'APZelos/blamer.nvim'

" A Vim plugin which shows git diff markers in the sign column and
" stages/previews/undoes hunks and partial hunks.
" Plug 'airblade/vim-gitgutter'

" Plug 'TimUntersberger/neogit'

" Colorscheme
Plug 'bluz71/vim-moonfly-colors'
Plug 'bluz71/vim-nightfly-guicolors'
Plug 'NLKNguyen/papercolor-theme'
Plug 'challenger-deep-theme/vim', { 'as': 'challenger-deep' }
Plug 'fenetikm/falcon'
" Plug 'projekt0n/github-nvim-theme'
Plug 'morhetz/gruvbox'
Plug 'sainnhe/sonokai'
Plug 'sainnhe/edge'
Plug 'sainnhe/everforest'
Plug 'sainnhe/gruvbox-material'
Plug 'EdenEast/nightfox.nvim'
Plug 'rafamadriz/neon'
Plug 'mhartington/oceanic-next'
Plug 'rose-pine/neovim'
Plug 'navarasu/onedark.nvim'
Plug 'folke/tokyonight.nvim', { 'branch': 'main' }
Plug 'nanotech/jellybeans.vim'
" Plug 'Everblush/everblush.nvim', { 'as': 'everblush' }
Plug 'preservim/vim-colors-pencil'
Plug 'junegunn/seoul256.vim'
Plug 'tomasr/molokai'
Plug 'ayu-theme/ayu-vim'
Plug 'catppuccin/nvim', { 'as': 'catppuccin' }
Plug 'rebelot/kanagawa.nvim'

" Remove all background colors to make nvim transparent
Plug 'xiyaowong/transparent.nvim'

" Adds file type icons to Vim plugins
Plug 'ryanoasis/vim-devicons'

" Surround.vim is all about "surroundings": parentheses, brackets, quotes,
" XML tags, and more. The plugin provides mappings to easily delete,
" change and add such surroundings in pairs.
" Plug 'tpope/vim-surround'

Plug 'kylechui/nvim-surround'

" repeat.vim: enable repeating supported plugin maps with "."
Plug 'tpope/vim-repeat'

" sleuth.vim: Heuristically set buffer options
Plug 'tpope/vim-sleuth'

Plug 'tpope/vim-unimpaired'

Plug 'tpope/vim-sensible'

" edit binary files by converting them to text equivalents
Plug 'tpope/vim-afterimage'

" Vim undo tree visualizer
" Plug 'simnalamburt/vim-mundo'

Plug 'mbbill/undotree'

Plug 'kana/vim-textobj-user'
Plug 'glts/vim-textobj-comment'
Plug 'sgur/vim-textobj-parameter'
Plug 'catppuccin/nvim', { 'as': 'catppuccin' }

" coc.nvim
" Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Or build from source code by using yarn: https://yarnpkg.com
" Plug 'neoclide/coc.nvim', {'branch': 'master', 'do': 'yarn install --frozen-lockfile'}

" eslint
" Plug 'eslint/eslint'
" Plug 'neoclide/coc-eslint'

" Plug 'terryma/vim-expand-region'

" Plug 'gcmt/wildfire.vim'

" Commentary
" vim-commentary
" Plug 'tpope/vim-commentary'

" Plug 'tomtom/tcomment_vim'

" Comment.nvim (tight integration with treesitter)
Plug 'numToStr/Comment.nvim'
" " Neovim treesitter plugin for setting the commentstring based on the cursor location in a file.
" " for jsx,tsx supported
Plug 'JoosepAlviste/nvim-ts-context-commentstring'

" vim-context-commentstring
Plug 'suy/vim-context-commentstring'

" vim-javascript
" JavaScript bundle for vim, this bundle provides syntax highlighting and improved indentation.
" Plug 'pangloss/vim-javascript'

" vim-jsx-pretty
" The React syntax highlighting and indenting plugin for vim. Also supports the typescript tsx file.
" Plug 'maxmellon/vim-jsx-pretty'

" ale
" Plug 'dense-analysis/ale'

" airline
" Plug 'vim-airline/vim-airline'

" feline
" Plug 'famiu/feline.nvim'

" lualine
Plug 'nvim-lualine/lualine.nvim'

" lsp-status
" Plug 'nvim-lua/lsp-status.nvim'

" lua `fork` of vim-web-devicons for neovim
Plug 'kyazdani42/nvim-web-devicons'

" Plug 'kyazdani42/nvim-tree.lua'

" The neovim tabline plugin
" Plug 'romgrk/barbar.nvim'

Plug 'neovim/nvim-lspconfig'
" Plug 'williamboman/nvim-lsp-installer'
Plug 'williamboman/mason.nvim'
Plug 'williamboman/mason-lspconfig.nvim'
" Plug 'hrsh7th/nvim-compe'
" Plug 'jose-elias-alvarez/null-ls.nvim'
" Plug 'jose-elias-alvarez/nvim-lsp-ts-utils'
Plug 'gfanto/fzf-lsp.nvim'
" Plug 'SmiteshP/nvim-navic'

Plug 'simrat39/rust-tools.nvim'

" TODO: config fidget
Plug 'j-hui/fidget.nvim'
" TODO: config lualine-lsp-progress
" Plug 'arkav/lualine-lsp-progress'

" Plug 'ray-x/lsp_signature.nvim'

Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-lua/popup.nvim'

Plug 'lewis6991/gitsigns.nvim'

Plug 'nvim-neorg/neorg'

" Plug 'glepnir/dashboard-nvim'

" vim-clap
" Plug 'liuchengxu/vim-clap'
Plug 'liuchengxu/vim-clap', { 'do': ':Clap install-binary' }
" Plug 'liuchengxu/vim-clap', { 'do': { -> clap#installer#force_download() } }

" automatically highlighting other uses of the word under the cursor
" Plug 'RRethy/vim-illuminate'

" nvim-cmp
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'petertriho/cmp-git'
Plug 'hrsh7th/nvim-cmp'
Plug 'ray-x/cmp-treesitter'

" Plug 'tzachar/cmp-tabnine', { 'do': './install.sh' }
" Plug 'codota/tabnine-nvim', { 'do': './dl_binaries.sh' }

" auto pairs
" Plug 'jiangmiao/auto-pairs'
Plug 'windwp/nvim-autopairs'

" luasnip.
Plug 'L3MON4D3/LuaSnip'
Plug 'saadparwaiz1/cmp_luasnip'
Plug 'rafamadriz/friendly-snippets'

" tabnine with nvim-cmp
" Plug 'tzachar/cmp-tabnine', { 'do': './install.sh' }

" vscode-like pictograms for neovim lsp completion items
Plug 'onsails/lspkind-nvim'

" Single tabpage interface for easily cycling through diffs for all modified
" files for any git rev.
" Plug 'nvim-lua/plenary.nvim'
Plug 'sindrets/diffview.nvim'

Plug 'mcchrish/nnn.vim'

" Plug 'preservim/nerdtree'
" Plug 'Xuyuanp/nerdtree-git-plugin'

" Plug 'lambdalisue/fern.vim', { 'branch': 'main' }

" Plug 'lambdalisue/fern-git-status.vim'

" " Make fern.vim as a default file explorer instead of Netrw
" Plug 'lambdalisue/fern-hijack.vim'
"
" Plug 'lambdalisue/fern-renderer-nerdfont.vim'
"
" Plug 'LumaKernel/fern-mapping-fzf.vim'

" Plug 'lambdalisue/fern-renderer-devicons.vim'
" Plug 'lambdalisue/fern-renderer-nerdfont.vim'

" Plug 'yuki-yano/fern-preview.vim'

" Plug 'tamago324/lir.nvim'

" Fundemental plugin to handle Nerd Fonts in Vim
Plug 'lambdalisue/nerdfont.vim'

" Fix CursorHold Performance for neovim
" TODO: maybe delete this in the feature
" Plug 'antoinemadec/FixCursorHold.nvim'

" Plug 'chentau/marks.nvim'

" Smooth scrolling neovim plugin written in lua
" Plug 'karb94/neoscroll.nvim'

" Indent guides for Neovim
" Plug 'lukas-reineke/indent-blankline.nvim'

" Vim motions on speed!
" Plug 'easymotion/vim-easymotion'

" Plug 'ThePrimeagen/harpoon'
Plug 'ThePrimeagen/harpoon', { 'branch': 'harpoon2' }

Plug 'nvim-telescope/telescope.nvim'

Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }

Plug 'nvim-telescope/telescope-ui-select.nvim'

" An extension for telescope.nvim that allows you operate zoxide within Neovim.
Plug 'jvgrootveld/telescope-zoxide'

Plug 'nvim-telescope/telescope-symbols.nvim'

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

Plug 'nvim-treesitter/nvim-treesitter-textobjects'

Plug 'nvim-treesitter/nvim-treesitter-context'

Plug 'nvim-treesitter/playground'

Plug 'norcalli/nvim-colorizer.lua'

" Plug 'ThePrimeagen/git-worktree.nvim'

Plug 'github/copilot.vim'

" Plug 'Exafunction/codeium.vim', { 'branch': 'main' }

" Plug 'voldikss/vim-floaterm'

Plug 'sbdchd/neoformat'

Plug 'WhoIsSethDaniel/toggle-lsp-diagnostics.nvim'

Plug 'bronson/vim-visual-star-search'

Plug 'dstein64/vim-startuptime'

Plug 'kevinhwang91/nvim-bqf'

Plug 'ThePrimeagen/git-worktree.nvim'

Plug 'Shatur/neovim-session-manager'

" Plugin to toggle, display and navigate marks
Plug 'kshenoy/vim-signature'

" VIM Table Mode for instant table creation.
Plug 'dhruvasagar/vim-table-mode'

Plug 'lervag/vimtex'

" https://github.com/dense-analysis/neural
" Vim/Neovim AI code generation plugin (OpenAI, ChatGPT, and more)
" Plug 'dense-analysis/neural'
" Plug 'muniftanjim/nui.nvim'
" Plug 'elpiloto/significant.nvim'

Plug 'folke/neodev.nvim'

Plug 'folke/zen-mode.nvim'

Plug 'https://github.com/apple/pkl-neovim.git'

" Plug 'tversteeg/registers.nvim', { 'branch': 'main' }

Plug 'stevearc/oil.nvim'

" Initialize plugin system
call plug#end()


let g:mapleader=","
" let g:mapleader="<Space>"





" " nvim window bar config --------------------
" lua <<EOF
" local function status_line()
"   local mode = "%-5{%v:lua.string.upper(v:lua.vim.fn.mode())%}"
"   local file_name = "%-.16t"
"   local buf_nr = "[%n]"
"   local modified = " %-m"
"   local file_type = " %y"
"   local right_align = "%="
"   local line_no = "%10([%l/%L%)]"
"   local pct_thru_file = "%5p%%"

"   return string.format(
"     "%s%s%s%s%s%s%s%s",
"     mode,
"     file_name,
"     buf_nr,
"     modified,
"     file_type,
"     right_align,
"     line_no,
"     pct_thru_file
"   )
" end
" -- vim.opt.statusline = status_line()
" vim.opt.winbar = status_line()
" EOF
" " nvim window bar config --------------------





" feline config --------------------
" lua require('feline').setup()
" feline config --------------------




" " indent-blankline.nvim config --------------------
" lua << END
" vim.opt.list = true
" -- vim.opt.listchars = {eol = '↴', , tab = '~>'}
" -- vim.opt.listchars = { eol = '↴', tab = '~>' }
" vim.opt.listchars = { eol = '↴', tab = '  ' }
" -- vim.opt.list = true
" -- -- vim.opt.listchars:append("space:⋅")
" -- vim.opt.listchars:append("eol:↴")
" -- 
" require("indent_blankline").setup {
"     space_char_blankline = " ",
"     show_current_context = true,
"     show_current_context_start = true,
"     show_end_of_line = true,
" }
" END
" " indent-blankline.nvim config --------------------




" lualine config --------------------
lua << END
local colors = {
  yellow   = '#F0E130',
  cyan     = '#07B8D5',
  darkblue = '#081633',
  green    = '#90EE90',
  orange   = '#FF8800',
  violet   = '#a9a1e1',
  magenta  = '#c678dd',
  blue     = '#91d5ff',
  red      = '#ec5f67',
}

require'lualine'.setup({
    options = {
        icons_enabled = true,
        theme = 'auto',
        section_separators = { left = '', right = ''},
        -- component_separators = { left = '', right = ''},
        -- component_separators = { left = '', right = ''},
        component_separators = '',
    },
    sections = {
      -- lualine_a = {'mode'},
      lualine_a = {{
          'mode',
          -- fmt = function(str) return str:sub(1,1) end,
          -- separator = { left = '▓' },
          separator = { left = '' },
          right_padding = 2,
          filetype_names = {
              TelescopePrompt = 'Telescope',
              dashboard = 'Dashboard',
              fzf = 'FZF',
          },
      }},
      lualine_b = {'branch', {
          -- 'diff',
          -- Is it me or the symbol for modified us really weird
          -- symbols = { added = '', modified = '柳', removed = '' },
          diff_color = {
              added = { fg = colors.green },
              modified = { fg = colors.yellow },
              removed = { fg = colors.red },
          },
      }, {
        'diagnostics',
        sources = { 'nvim_diagnostic' },
        sections = { 'error', 'warn', 'info', 'hint' },
        -- symbols = { error = ' ', warn = ' ', info = ' ' },
        symbols = {error = 'E', warn = 'W', info = 'I', hint = 'H'},
        colored = true,
        diagnostics_color = {
          error = { fg = colors.red },
          warn = { fg = colors.yellow },
          info = { fg = colors.cyan },
          hint = { fg = colors.blue }
        },
        update_in_insert = false,
        always_visible = false,
      }},
      -- lualine_c = {'filename', 'filesize', "require'lsp-status'.status()"},
      lualine_c = {
        {
          'filename',
          file_status = true,      -- Displays file status (readonly status, modified status)
          path = 1,                -- 0: Just the filename
                                   -- 1: Relative path
                                   -- 2: Absolute path

          shorting_target = 1,    -- Shortens path to leave 40 spaces in the window
                                   -- for other components. (terrible name, any suggestions?)
          symbols = {
            modified = '[+]',      -- Text to show when the file is modified.
            -- modified = '[●]',      -- Text to show when the file is modified.
            readonly = '[-]',      -- Text to show when the file is non-modifiable or readonly.
            unnamed = '[No Name]', -- Text to show for unnamed buffers.
          }
        },
        'filesize',
        {
          function()
            return '%='
          end,
        }, {
          -- Lsp server name .
          function()
            -- local msg = 'No Active Lsp'
            local msg = '--'
            local buf_ft = vim.api.nvim_buf_get_option(0, 'filetype')
            local clients = vim.lsp.get_clients()
            if next(clients) == nil then
              return msg
            end
            for _, client in ipairs(clients) do
              local filetypes = client.config.filetypes
              if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
                return client.name
              end
            end
            return msg
          end,
          -- icon = ' LSP:',
          icon = '',
          color = { gui = 'bold' },
        }
      },
      lualine_x = {'encoding', {
        'fileformat',
        fmt = string.lower,
        icons_enabled = false, -- I think icons are cool but Eviline doesn't have them. sigh
        -- color = { fg = colors.green, gui = 'bold' },
        -- symbols = {
        --   unix = '', -- e712
        --   dos = '',  -- e70f
        --   mac = '',  -- e711
        -- }
      }, 'filetype'},
      lualine_y = {'progress'},
      lualine_z = {{
        'location',
        -- separator = { right = '▓' },
        separator = { right = '' },
        left_padding = 2
      }},
    },
    inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {'filename'},
        lualine_x = {'location'},
        lualine_y = {},
        lualine_z = {},
    },
    tabline = {},
    extensions = {},
    globalstatus = true,
})
END
" lualine config --------------------





" " " null-ls config --------------------
" lua <<EOF
" local null_ls = require("null-ls")

" local h = require("null-ls.helpers")
" local u = require("null-ls.utils")

" local eslint = null_ls.builtins.diagnostics.eslint.with({
"     cwd = h.cache.by_bufnr(function(params)
"         return u.root_pattern(
"             ".eslintrc",
"             ".eslintrc.js",
"             ".eslintrc.cjs",
"             ".eslintrc.yaml",
"             ".eslintrc.yml",
"             ".eslintrc.json"
"         )(params.bufname)
"     end),
" })

" null_ls.setup({
"     sources = {
"         null_ls.builtins.formatting.stylua,
"         -- null_ls.builtins.code_actions.eslint,
"         null_ls.builtins.diagnostics.eslint_d,
"         null_ls.builtins.formatting.eslint_d,
"         -- eslint,
"         null_ls.builtins.code_actions.eslint_d,
"         -- null_ls.builtins.completion.spell,
"     },
" })

" vim.api.nvim_create_user_command("NullLsToggle", function()
"     -- you can also create commands to disable or enable sources
"     require("null-ls").toggle({})
" end, {})

" EOF
" " " null-ls config --------------------




" clap config --------------------
" let g:clap_preview_direction = 'LR'
" " let g:clap_layout = {'width': '90%', 'col':'1%', 'height': '90%', 'row': '3%' }
" let g:clap_layout = {'relative': 'editor', 'width': '47%', 'height': '90%', 'col': '3%', 'row': '6%'}
" clap config --------------------




" nvim-lsp-installer config --------------------
" lua <<EOF
" require("nvim-lsp-installer").setup {}
" EOF
" nvim-lsp-installer config --------------------






" neodev.nvim config ----------------------
lua <<EOF
-- IMPORTANT: make sure to setup neodev BEFORE lspconfig
require("neodev").setup({
  -- add any options here, or leave empty to use the default settings
})
EOF
" neodev.nvim config ----------------------






" mason.nvim config --------------------
lua <<EOF
require("mason").setup()
EOF
" mason.nvim config --------------------





" lspconfig config --------------------
" mason-lspconfig.nvim config --------------------

" nnoremap <silent> <A-e> :lua require("harpoon.ui").nav_file(1)<CR>
nnoremap <leader>ls :LspStart 
nnoremap <leader>lt :LspStop<CR>
nnoremap <leader>lrs :LspRestart<CR>

lua << EOF
local lspconfig = require('lspconfig')
local util = require('lspconfig/util')
-- local navic = require('nvim-navic')
-- local lsp_signature = require('lsp_signature')

-- navic.setup {
--     icons = {
--         File          = " ",
--         Module        = " ",
--         Namespace     = " ",
--         Package       = " ",
--         Class         = " ",
--         Method        = " ",
--         Property      = " ",
--         Field         = " ",
--         Constructor   = " ",
--         Enum          = "練",
--         Interface     = "練",
--         Function      = " ",
--         Variable      = " ",
--         Constant      = " ",
--         String        = " ",
--         Number        = " ",
--         Boolean       = "◩ ",
--         Array         = " ",
--         Object        = " ",
--         Key           = " ",
--         Null          = "ﳠ ",
--         EnumMember    = " ",
--         Struct        = " ",
--         Event         = " ",
--         Operator      = " ",
--         TypeParameter = " ",
--     },
--     highlight = false,
--     separator = " > ",
--     depth_limit = 0,
--     depth_limit_indicator = "..",
--     safe_output = true
-- }

-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
-- local opts = { noremap=true, silent=true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)

-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    -- Enable completion triggered by <c-x><c-o>
    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- Buffer local mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local opts = { buffer = ev.buf }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
    vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
    vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
    vim.keymap.set('n', '<space>wl', function()
      print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, opts)
    vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
    vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', '<space>f', function()
      vim.lsp.buf.format { async = true }
    end, opts)
  end,
})

-- local on_attach = function(client, bufnr)
--   -- Enable completion triggered by <c-x><c-o>
--   vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

--   -- Mappings.
--   -- See `:help vim.lsp.*` for documentation on any of the below functions
--   local bufopts = { noremap=true, silent=true, buffer=bufnr }
--   vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
--   vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
--   vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
--   vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
--   vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
--   vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
--   vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
--   vim.keymap.set('n', '<space>wl', function()
--     print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
--   end, bufopts)
--   vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
--   vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
--   vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
--   vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
--   vim.keymap.set('n', '<space>f', function() vim.lsp.buf.format { async = true } end, bufopts)

--   -- vim-illuminate config --------------------
--   -- require 'illuminate'.on_attach(client)
--   -- vim-illuminate config --------------------
-- end

local lsp_flags = {
  -- This is the default in Nvim 0.7+
  debounce_text_changes = 150,
}

local prettier = {
   -- formatCommand = "prettier --stdin-filepath ${INPUT}",
   -- prettierd is more faster
   formatCommand = 'prettierd "${INPUT}"',
   formatStdin = true,
   env = {
     string.format('PRETTIERD_DEFAULT_CONFIG=%s', vim.fn.expand('~/.config/nvim/utils/linter-config/.prettierrc.json')),
   },
}

local eslint = {
   lintCommand = "eslint_d -f unix --stdin --stdin-filename ${INPUT}",
   lintIgnoreExitCode = true,
   lintStdin = true,
   lintFormats = {"%f:%l:%c: %m"},
   lintIgnoreExitCode = true,
   formatCommand = "eslint_d --fix-to-stdout --stdin --stdin-filename=${INPUT}",
   formatStdin = true,
  --  env = {
  --   string.format("ESLINT_CONFIG=%s", vim.fn.expand("~/.config/nvim/utils/linter-config/.eslintrc.js")),
  -- },
}

local capabilities = require('cmp_nvim_lsp').default_capabilities()

require("mason-lspconfig").setup {
  ensure_installed = { "lua_ls",  "rust_analyzer", "ts_ls", "gopls", "volar", "efm", "phpactor" },
  automatic_installation = false,
}
require("mason-lspconfig").setup_handlers {
  -- The first entry (without a key) will be the default handler
  -- and will be called for each installed server that doesn't have
  -- a dedicated handler.
  function (server_name) -- default handler (optional)
    -- print("setup lspconfig for " .. server_name)
    require("lspconfig")[server_name].setup {
      -- on_attach = on_attach,
      flags = lsp_flags,
      capabilities = capabilities,
      autostart = false,
    }
  end,
  -- Next, you can provide a dedicated handler for specific servers.
  -- For example, a handler override for the `rust_analyzer`:
  ["lua_ls"] = function ()
    lspconfig["lua_ls"].setup {
      capabilities = capabilities,
      settings = {
        Lua = {
          completion = {
            callSnippet = "Replace"
          },
          diagnostics = {
            globals = { "vim" }
          }
        }
      },
      autostart = false,
    }
  end,
  ["rust_analyzer"] = function ()
    require("rust-tools").setup {}
    lspconfig["rust_analyzer"].setup{
      capabilities = capabilities,
      -- on_attach = on_attach,
      flags = lsp_flags,
      -- Server-specific settings...
      settings = {
        ["rust-analyzer"] = {
          assist = {
            importGranularity = "module",
            importPrefix = "by_self",
          },
          cargo = {
            loadOutDirsFromCheck = true
          },
          procMacro = {
            enable = true
          },
        }
      }
    }
  end,
  ["gopls"] = function ()
    lspconfig["gopls"].setup{
      capabilities = capabilities,
      -- on_attach = on_attach,
      flags = lsp_flags,
    }
  end,
  ["pyright"] = function ()
    lspconfig["pyright"].setup{
      capabilities = capabilities,
      -- on_attach = on_attach,
      flags = lsp_flags,
      settings = {
          pyright = {
              -- 启用严格的类型检查
              typeCheckingMode = "basic"
          },
          python = {
              analysis = {
                  -- 自动导入和类型推断
                  autoImportCompletions = true,
                  autoSearchPaths = true,
                  diagnosticMode = "workspace",
                  useLibraryCodeForTypes = true
              }
          }
      },
      root_dir = lspconfig.util.root_pattern('.git', 'pyrightconfig.json', 'pyproject.toml')
    }
  end,
  ["ts_ls"] = function ()
    lspconfig["ts_ls"].setup{
      capabilities = capabilities,
      on_attach = function(client, bufnr)
        client.server_capabilities.document_formatting = false
        client.server_capabilities.documentFormattingProvider = false
        client.server_capabilities.documentRangeFormattingProvider = false
        -- on_attach(client, bufnr)
      end,
      flags = lsp_flags,
      -- settings = {
      -- },
      autostart = false,
    }
  end,
  -- ["tailwindcss"] = function ()
  --   lspconfig["tailwindcss"].setup{
  --     capabilities = capabilities,
  --     on_attach = on_attach,
  --     flags = lsp_flags,
  --     autostart = false,
  --   }
  -- end,
  ["volar"] = function ()
    lspconfig["volar"].setup{
      capabilities = capabilities,
      -- on_attach = on_attach,
      on_attach = function(client, bufnr)
        client.server_capabilities.document_formatting = false
        client.server_capabilities.documentFormattingProvider = false
        client.server_capabilities.documentRangeFormattingProvider = false
        -- on_attach(client, bufnr)
      end,
      flags = lsp_flags,
      filetypes = {'typescript', 'javascript', 'javascriptreact', 'typescriptreact', 'vue', 'json'},
      autostart = false,
    }
  end,
  ["efm"] = function ()
    lspconfig["efm"].setup {
        init_options = {documentFormatting = true},
        -- on_attach = on_attach,
        root_dir = function(fname)
          return util.root_pattern(".git")(fname) or vim.fn.getcwd()
        end;
        settings = {
            rootMarkers = {
              ".git/",
              "package.json",
              ".eslintrc.js",
              ".eslintrc.json",
              ".eslintrc.yaml",
              ".eslintrc.yml",
              ".eslintrc",
              ".eslintrc.cjs"
            },
            languages = {
                lua = {
                    {formatCommand = "lua-format -i", formatStdin = true}
                },
                vue = { eslint },
                javascript = { eslint },
                typescript = { eslint },
                typescriptreact = { eslint },
                javascriptreact = { eslint },
                yaml = { prettier },
                json = { prettier },
                html = { prettier },
                less = { prettier },
                scss = { prettier },
                css = { prettier },
                markdown = { prettier },
            },
            filetypes = {
                "javascript",
                "typescript",
                "javascriptreact",
                "typescriptreact",
                "less",
                "scss",
                "css",
                "html",
                "json",
                "yaml",
                "python",
            },
        }
    }
  end,
}

-- require('lspconfig')['efm'].setup{
--     on_attach = on_attach,
--     rootMarkers = {".git/"},
--     languages = {
--         lua = {
--             {formatCommand = "lua-format -i", formatStdin = true}
--         },
--         javascript = { eslint },
--         typescript = { eslint },
--         typescriptreact = {
--             {
--                 lintCommand = "eslint_d -f unix --stdin --stdin-filename ${INPUT}",
--                 lintStdin = true,
--                 lintFormats = {"%f:%l:%c: %m"},
--                 lintIgnoreExitCode = true,
--                 formatCommand = "eslint_d --fix-to-stdout --stdin --stdin-filename=${INPUT}",
--                 formatStdin = true,
--             }
--         },
--         javascriptreact = { eslint },
--         yaml = { prettier },
--         json = { prettier },
--         html = { prettier },
--         less = { prettier },
--         scss = { prettier },
--         css = { prettier },
--         markdown = { prettier },
--     },
--     filetypes = {
--         "javascript",
--         "typescript",
--         "javascriptreact",
--         "typescriptreact",
--         "less",
--         "scss",
--         "css",
--         "html",
--         "json",
--         "yaml",
--         "python",
--     },
-- }

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
-- local servers = { 'pyright', 'rust_analyzer', 'gopls', 'tsserver', 'cssls', 'sumneko_lua', 'efm' }
-- for _, lsp in ipairs(servers) do
--   -- local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
--   config = {
--     autostart = false,
--     on_attach = on_attach,
--     flags = {
--       debounce_text_changes = 150,
--     },
--     -- capabilities = capabilities,
--   }

--   if (lsp == 'rust_analyzer') then
--     config['settings'] = {
--       ["rust-analyzer"] = {
--         assist = {
--           importGranularity = "module",
--           importPrefix = "by_self",
--         },
--         cargo = {
--           loadOutDirsFromCheck = true
--         },
--         procMacro = {
--           enable = true
--         },
--       }
--     }
--   end
--   if (lsp == 'sumneko_lua') then
--     config['setting'] = {
--       Lua = {
--         workspace = {
--           library = {
--             "/usr/share/nvim/runtime/lua",
--             "/usr/share/nvim/runtime/lua/vim",
--             "/usr/share/nvim/runtime/lua/vim/lsp",
--           },
--         },
--         -- TODO: have no idea why this is not working ???
--         diagnostics = {
--           globals = { 'vim' }
--         }
--       }
--     }
--   end
--   if (lsp == 'efm') then
--     config['init_options'] = {documentFormatting = true}
--     config['autostart'] = true
--     config['setting'] = {
--       rootMarkers = {".git/"},
--       languages = {
--           lua = {
--               {formatCommand = "lua-format -i", formatStdin = true}
--           },
--           javascript = { eslint },
--           typescript = { eslint },
--           typescriptreact = {
--               {
--                   lintCommand = "eslint_d -f unix --stdin --stdin-filename ${INPUT}",
--                   lintStdin = true,
--                   lintFormats = {"%f:%l:%c: %m"},
--                   lintIgnoreExitCode = true,
--                   formatCommand = "eslint_d --fix-to-stdout --stdin --stdin-filename=${INPUT}",
--                   formatStdin = true,
--               }
--           },
--           javascriptreact = { eslint },
--           -- yaml = { prettier },
--           -- json = { prettier },
--           -- html = { prettier },
--           -- less = { prettier },
--           -- scss = { prettier },
--           -- css = { prettier },
--           -- markdown = { prettier },
--       },
--       filetypes = {
--           "javascript",
--           "javascriptreact",
--           "javascript.jsx",
--           "typescript",
--           "typescriptreact",
--           "typescript.tsx",
--       },
--     }
--   end
--   -- if (lsp == 'null_ls') then
--   --   config['autostart'] = true
--   -- end

--   nvim_lsp[lsp].setup(config)
-- end
EOF
" mason-lspconfig.nvim config --------------------
" lspconfig config --------------------





" gitsigns config --------------------
lua require('gitsigns').setup()
" gitsigns config --------------------




" " vim-illuminate config --------------------
" " let g:Illuminate_ftblacklist = ['nerdtree']
" hi illuminatedWord cterm=underline gui=underline
" " hi illuminatedWordk ctermbg=4 guibg=#8b0000
" " vim-illuminate config --------------------




" neoscroll config --------------------
" lua require('neoscroll').setup()
" neoscroll config --------------------




" " ale config --------------------
" " Fix files with then ESLint.
" " let b:ale_fixers = ['eslint']
" let g:ale_linters = {'rust': ['analyzer']}
" " In ~/.vim/vimrc, or somewhere similar.
" let g:ale_fixers = {
" \   '*': ['remove_trailing_lines', 'trim_whitespace'],
" \   'javascript': ['eslint'],
" \   'typescript': ['eslint'],
" \   'typescriptreact': ['eslint'],
" \}
" " Set this variable to 1 to fix files when you save them.
" " let g:ale_fix_on_save = 1

" let g:lightline = {
"       \ 'colorscheme': 'nightfly',
"       \ }
" " ale config --------------------




" airline config --------------------
" let g:airline_powerline_fonts = 1 
" airline config --------------------




" fzf.vim config --------------------
" let g:fzf_layout = { 'window': { 'width': 0.8, 'height': 0.8 } }
let g:fzf_layout = { 'down': '30%' }
autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=3 showmode ruler
" let $FZF_DEFAULT_OPTS='--reverse'

function! s:update_fzf_colors()
  let rules =
  \ { 'fg':      [['Normal',       'fg']],
    \ 'bg':      [['Normal',       'bg']],
    \ 'hl':      [['Comment',      'fg']],
    \ 'fg+':     [['CursorColumn', 'fg'], ['Normal', 'fg']],
    \ 'bg+':     [['CursorColumn', 'bg']],
    \ 'hl+':     [['Statement',    'fg']],
    \ 'info':    [['PreProc',      'fg']],
    \ 'prompt':  [['Conditional',  'fg']],
    \ 'pointer': [['Exception',    'fg']],
    \ 'marker':  [['Keyword',      'fg']],
    \ 'spinner': [['Label',        'fg']],
    \ 'header':  [['Comment',      'fg']] }
  let cols = []
  for [name, pairs] in items(rules)
    for pair in pairs
      let code = synIDattr(synIDtrans(hlID(pair[0])), pair[1])
      if !empty(name) && code > 0
        call add(cols, name.':'.code)
        break
      endif
    endfor
  endfor
  let s:orig_fzf_default_opts = get(s:, 'orig_fzf_default_opts', $FZF_DEFAULT_OPTS)
  let $FZF_DEFAULT_OPTS = s:orig_fzf_default_opts .
        \ empty(cols) ? '' : (' --color='.join(cols, ','))
endfunction

augroup _fzf
  autocmd!
  autocmd ColorScheme * call <sid>update_fzf_colors()
augroup END

let g:fzf_history_dir = '~/.local/share/fzf-history'

" nnoremap <leader>ff :Files<cr>
" nnoremap <leader>fb :Buffers<cr>
" nnoremap <leader>gf :GFiles<cr>
nnoremap <leader>dn :Diagnostics<cr>
" fzf.vim config --------------------




" fzf-checkout config --------------------
" nnoremap <leader>gc :GCheckout<CR>
" fzf-checkout config --------------------




" barbar config --------------------
" let bufferline = get(g:, 'bufferline', {})
" " Enable/disable animations
" let bufferline.animation = v:false
" " Enable/disable auto-hiding the tab bar when there is a single buffer
" " let bufferline.auto_hide = v:true
" " Configure icons on the bufferline.
" let bufferline.icon_separator_active = '▎'
" let bufferline.icon_separator_inactive = '▎'
" let bufferline.icon_close_tab = ''
" let bufferline.icon_close_tab_modified = '●'
" let bufferline.icon_pinned = '車'

"" Move to previous/next
"nnoremap <silent>    <A-,> :BufferPrevious<CR>
"nnoremap <silent>    <A-.> :BufferNext<CR>
"" Re-order to previous/next
"nnoremap <silent>    <A-<> :BufferMovePrevious<CR>
"nnoremap <silent>    <A->> :BufferMoveNext<CR>
"" Goto buffer in position...
"nnoremap <silent>    <A-1> :BufferGoto 1<CR>
"nnoremap <silent>    <A-2> :BufferGoto 2<CR>
"nnoremap <silent>    <A-3> :BufferGoto 3<CR>
"nnoremap <silent>    <A-4> :BufferGoto 4<CR>
"nnoremap <silent>    <A-5> :BufferGoto 5<CR>
"nnoremap <silent>    <A-6> :BufferGoto 6<CR>
"nnoremap <silent>    <A-7> :BufferGoto 7<CR>
"nnoremap <silent>    <A-8> :BufferGoto 8<CR>
"nnoremap <silent>    <A-9> :BufferLast<CR>
"" Pin/unpin buffer
"nnoremap <silent>    <A-p> :BufferPin<CR>
"" Close buffer
"nnoremap <silent>    <A-c> :BufferClose<CR>
" Wipeout buffer
"                          :BufferWipeout<CR>
" Close commands
"                          :BufferCloseAllButCurrent<CR>
"                          :BufferCloseAllButPinned<CR>
"                          :BufferCloseBuffersLeft<CR>
"                          :BufferCloseBuffersRight<CR>
" Magic buffer-picking mode
" nnoremap <silent> <C-s>    :BufferPick<CR>
" Sort automatically by...
" nnoremap <silent> <Space>bb :BufferOrderByBufferNumber<CR>
" nnoremap <silent> <Space>bd :BufferOrderByDirectory<CR>
" nnoremap <silent> <Space>bl :BufferOrderByLanguage<CR>
" nnoremap <silent> <Space>bw :BufferOrderByWindowNumber<CR>
" Other:
" :BarbarEnable - enables barbar (enabled by default)
" :BarbarDisable - very bad command, should never be used
"
" highlighting
"let fg_target = 'red'

"let fg_current  = s:fg(['Normal'], '#efefef')
"let fg_visible  = s:fg(['TabLineSel'], '#efefef')
"let fg_inactive = s:fg(['TabLineFill'], '#888888')

"let fg_modified  = s:fg(['WarningMsg'], '#E5AB0E')
"let fg_special  = s:fg(['Special'], '#599eff')
"let fg_subtle  = s:fg(['NonText', 'Comment'], '#555555')

"let bg_current  = s:bg(['Normal'], '#000000')
"let bg_visible  = s:bg(['TabLineSel', 'Normal'], '#000000')
"let bg_inactive = s:bg(['TabLineFill', 'StatusLine'], '#000000')

"" Meaning of terms:
""
"" format: "Buffer" + status + part
""
"" status:
""     *Current: current buffer
""     *Visible: visible but not current buffer
""    *Inactive: invisible but not current buffer
""
"" part:
""        *Icon: filetype icon
""       *Index: buffer index
""         *Mod: when modified
""        *Sign: the separator between buffers
""      *Target: letter in buffer-picking mode
""
"" BufferTabpages: tabpage indicator
"" BufferTabpageFill: filler after the buffer section
"" BufferOffset: offset section, created with set_offset()

"call s:hi_all([
"\ ['BufferCurrent',        fg_current,  bg_current],
"\ ['BufferCurrentIndex',   fg_special,  bg_current],
"\ ['BufferCurrentMod',     fg_modified, bg_current],
"\ ['BufferCurrentSign',    fg_special,  bg_current],
"\ ['BufferCurrentTarget',  fg_target,   bg_current,   'bold'],
"\ ['BufferVisible',        fg_visible,  bg_visible],
"\ ['BufferVisibleIndex',   fg_visible,  bg_visible],
"\ ['BufferVisibleMod',     fg_modified, bg_visible],
"\ ['BufferVisibleSign',    fg_visible,  bg_visible],
"\ ['BufferVisibleTarget',  fg_target,   bg_visible,   'bold'],
"\ ['BufferInactive',       fg_inactive, bg_inactive],
"\ ['BufferInactiveIndex',  fg_subtle,   bg_inactive],
"\ ['BufferInactiveMod',    fg_modified, bg_inactive],
"\ ['BufferInactiveSign',   fg_subtle,   bg_inactive],
"\ ['BufferInactiveTarget', fg_target,   bg_inactive,  'bold'],
"\ ['BufferTabpages',       fg_special,  bg_inactive, 'bold'],
"\ ['BufferTabpageFill',    fg_inactive, bg_inactive],
"\ ])

"call s:hi_link([
"\ ['BufferCurrentIcon',  'BufferCurrent'],
"\ ['BufferVisibleIcon',  'BufferVisible'],
"\ ['BufferInactiveIcon', 'BufferInactive'],
"\ ['BufferOffset',       'BufferTabpageFill'],
"\ ])

"" NOTE: this is an example taken from the source, implementation of
"" s:fg(), s:bg(), s:hi_all() and s:hi_link() is left as an exercise
"" for the reader.
" barbar config --------------------




" nvim tree config  --------------------
"lua require'nvim-tree'.setup()
"let g:nvim_tree_indent_markers = 1 "0 by default, this option shows indent markers when folders are open
"let g:nvim_tree_git_hl = 1 "0 by default, will enable file highlight for git attributes (can be used without the icons).
"let g:nvim_tree_highlight_opened_files = 1 "0 by default, will enable folder and file icon highlight for opened files/directories.
"let g:nvim_tree_root_folder_modifier = ':~' "This is the default. See :help filename-modifiers for more options
"let g:nvim_tree_add_trailing = 1 "0 by default, append a trailing slash to folder names
"let g:nvim_tree_group_empty = 1 " 0 by default, compact folders that only contain a single folder into one node in the file tree
"let g:nvim_tree_disable_window_picker = 1 "0 by default, will disable the window picker.
"let g:nvim_tree_icon_padding = ' ' "one space by default, used for rendering the space between the icon and the filename. Use with caution, it could break rendering if you set an empty string depending on your font.
"let g:nvim_tree_symlink_arrow = ' >> ' " defaults to ' ➛ '. used as a separator between symlinks' source and target.
"let g:nvim_tree_respect_buf_cwd = 1 "0 by default, will change cwd of nvim-tree to that of new buffer's when opening nvim-tree.
"let g:nvim_tree_create_in_closed_folder = 0 "1 by default, When creating files, sets the path of a file when cursor is on a closed folder to the parent folder when 0, and inside the folder when 1.
"let g:nvim_tree_window_picker_exclude = {
"    \   'filetype': [
"    \     'notify',
"    \     'packer',
"    \     'qf'
"    \   ],
"    \   'buftype': [
"    \     'terminal'
"    \   ]
"    \ }
"" Dictionary of buffer option names mapped to a list of option values that
"" selectable.
"let g:nvim_tree_special_files = { 'README.md': 1, 'Makefile': 1, 'MAKEFILE': 1 } " List of filenames that gets highlighted with NvimTreeSpecialFile
"let g:nvim_tree_show_icons = {
"    \ 'git': 1,
"    \ 'folders': 0,
"    \ 'files': 0,
"    \ 'folder_arrows': 0,
"    \ }
""If 0, do not show the icons for one of 'git' 'folder' and 'files'
""1 by default, notice that if 'files' is 1, it will only display
""if nvim-web-devicons is installed and on your runtimepath.
""if folder is 1, you can also tell folder_arrows 1 to show small arrows next to the folder icons.
""but this will not work when you set indent_markers (because of UI conflict)

"" default will show icon by default if no icon is provided
"" default shows no icon by default
"let g:nvim_tree_icons = {
"    \ 'default': '',
"    \ 'symlink': '',
"    \ 'git': {
"    \   'unstaged': "✗",
"    \   'staged': "✓",
"    \   'unmerged': "",
"    \   'renamed': "➜",
"    \   'untracked': "★",
"    \   'deleted': "",
"    \   'ignored': "◌"
"    \   },
"    \ 'folder': {
"    \   'arrow_open': "",
"    \   'arrow_closed': "",
"    \   'default': "",
"    \   'open': "",
"    \   'empty': "",
"    \   'empty_open': "",
"    \   'symlink': "",
"    \   'symlink_open': "",
"    \   }
"    \ }

"nnoremap <C-n> :NvimTreeToggle<CR>
"nnoremap <leader>r :NvimTreeRefresh<CR>
"nnoremap <leader>n :NvimTreeFindFile<CR>

"highlight NvimTreeFolderIcon guibg=blue
" nvim tree config  --------------------




" nvim-cmp config --------------------
" cmp-tabnine config --------------------
" lua require'lsp/nvim-cmp'
lua <<END
  -- Setup nvim-cmp.
  -- local cmp_autopairs = require('nvim-autopairs.completion.cmp')
  local cmp = require('cmp')
  -- local tabnine = require('cmp_tabnine.config')
  -- cmp.event:on(
  --   'confirm_done',
  --   cmp_autopairs.on_confirm_done()
  -- )
  local kind_icons = {
    Text = "",
    Method = "",
    Function = "",
    Constructor = "",
    Field = "",
    Variable = "",
    Class = "ﴯ",
    Interface = "",
    Module = "",
    Property = "ﰠ",
    Unit = "",
    Value = "",
    Enum = "",
    Keyword = "",
    Snippet = "",
    Color = "",
    File = "",
    Reference = "",
    Folder = "",
    EnumMember = "",
    Constant = "",
    Struct = "",
    Event = "",
    Operator = "",
    TypeParameter = ""
  }

  local function send_wildchar()
    local char = vim.fn.nr2char(vim.opt.wildchar:get())
    local key = vim.api.nvim_replace_termcodes(char, true, false, true)
    vim.api.nvim_feedkeys(key, "nt", true)
  end

  cmp.setup({
    formatting = {
      format = function(entry, vim_item)
        -- Kind icons
        vim_item.kind = string.format('%s %s', kind_icons[vim_item.kind], vim_item.kind) -- This concatonates the icons with the name of the item kind
        -- Source
        vim_item.menu = ({
          buffer = "[Buffer]",
          nvim_lsp = "[LSP]",
          luasnip = "[LuaSnip]",
          nvim_lua = "[Lua]",
          latex_symbols = "[LaTeX]",
          path = "[Path]",
          -- cmp_tabnine = "[TN]",
        })[entry.source.name]

        -- tabnine setup ----------
        -- local menu = source_mapping[entry.source.name]
        if entry.source.name == 'cmp_tabnine' then
          if entry.completion_item.data ~= nil and entry.completion_item.data.detail ~= nil then
            vim_item.menu = entry.completion_item.data.detail .. ' ' .. vim_item.menu
          end
          vim_item.kind = ''
        end
        -- tabnine setup ----------

        return vim_item
      end
    },
    snippet = {
      -- REQUIRED - you must specify a snippet engine
      expand = function(args)
        -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
        require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
        -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
        -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
      end,
    },
    window = {
      completion = cmp.config.window.bordered(),
      -- documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
      ['<C-b>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping.abort(),
      ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    }),
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      -- { name = 'vsnip' }, -- For vsnip users.
      { name = 'luasnip' }, -- For luasnip users.
      -- { name = 'ultisnips' }, -- For ultisnips users.
      -- { name = 'snippy' }, -- For snippy users.
      -- { name = 'cmp_tabnine' }, -- For snippy users.
      { name = "treesitter" },
    }, {
      { name = 'buffer' },
      { name = "path" },
    })
  })
  -- Set configuration for specific filetype.
  cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
      { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
    }, {
      { name = 'buffer' },
    })
  })

  -- -- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
  -- cmp.setup.cmdline('/', {
  --   mapping = cmp.mapping.preset.cmdline(),
  --   sources = {
  --     { name = 'buffer' }
  --   }
  -- })

  -- -- disable cmp-cmdline in :
  -- -- ref from https://github.com/hrsh7th/cmp-cmdline/issues/52#issuecomment-1704355620
  -- cmp.setup.cmdline(':', {
  --   mapping = {
  --       ["<Tab>"] = {c = send_wildchar}
  --     },
  --   sources = cmp.config.sources({})
  --   -- mapping = cmp.mapping.preset.cmdline(),
  --   -- sources = cmp.config.sources({
  --   --   { name = 'path' }
  --   -- }, {
  --   --   { name = 'cmdline' }
  --   -- })
  -- })

  -- -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  -- cmp.setup.cmdline(':', {
  --   mapping = cmp.mapping.preset.cmdline(),
  --   sources = cmp.config.sources({
  --     { name = 'path' }
  --   }, {
  --     { name = 'cmdline' }
  --   })
  -- })

  -- tabnine.setup({
  --   max_lines = 1000,
  --   max_num_results = 20,
  --   sort = true,
  --   run_on_every_keystroke = true,
  --   snippet_placeholder = '..',
  --   ignored_file_types = { 
  --     -- default is not to ignore
  --     -- uncomment to ignore in lua:
  --     -- lua = true
  --   },
  --   show_prediction_strength = false
  -- })

END

" set completeopt=menu,menuone,noselect

" cmp-tabnine config --------------------
" nvim-cmp config --------------------





" easymotion config --------------------
" nmap ss <Plug>(easymotion-s2)
" easymotion config --------------------





" nerdtree config  --------------------
" nerdtree-git-plugin config
" let g:NERDTreeGitStatusIndicatorMapCustom = {
"                 \ 'Modified'  :'✹',
"                 \ 'Staged'    :'✚',
"                 \ 'Untracked' :'✭',
"                 \ 'Renamed'   :'➜',
"                 \ 'Unmerged'  :'═',
"                 \ 'Deleted'   :'✖',
"                 \ 'Dirty'     :'✗',
"                 \ 'Ignored'   :'☒',
"                 \ 'Clean'     :'✔︎',
"                 \ 'Unknown'   :'?',
"                 \ }
" nerdtree config  --------------------





" marks.nvim config --------------------
" lua << END
" require'marks'.setup {
"   -- whether to map keybinds or not. default true
"   default_mappings = true,
"   -- which builtin marks to show. default {}
"   builtin_marks = { ".", "<", ">", "^" },
"   -- whether movements cycle back to the beginning/end of buffer. default true
"   cyclic = true,
"   -- whether the shada file is updated after modifying uppercase marks. default false
"   force_write_shada = false,
"   -- how often (in ms) to redraw signs/recompute mark positions.
"   -- higher values will have better performance but may cause visual lag,
"   -- while lower values may cause performance penalties. default 150.
"   refresh_interval = 250,
"   -- sign priorities for each type of mark - builtin marks, uppercase marks, lowercase
"   -- marks, and bookmarks.
"   -- can be either a table with all/none of the keys, or a single number, in which case
"   -- the priority applies to all marks.
"   -- default 10.
"   sign_priority = { lower=10, upper=15, builtin=8, bookmark=20 },
"   -- disables mark tracking for specific filetypes. default {}
"   excluded_filetypes = { "unix", "fzf", "fern", "floaterm", "TERMINAL" },
"   -- marks.nvim allows you to configure up to 10 bookmark groups, each with its own
"   -- sign/virttext. Bookmarks can be used to group together positions and quickly move
"   -- across multiple buffers. default sign is '!@#$%^&*()' (from 0 to 9), and
"   -- default virt_text is "".
"   bookmark_0 = {
"     sign = "⚑",
"     virt_text = "hello world"
"   },
"   mappings = {}
" }
" END
" marks.nvim config --------------------





" harpoon config --------------------
" lua <<END
" require("harpoon").setup({
"     menu = {
"         width = vim.api.nvim_win_get_width(0) - 70,
"         height = 16,
"     }
" })
" END
" noremap <silent> <C-p> :lua require("harpoon.ui").toggle_quick_menu()<CR>
" " noremap <silent> <C-j> :lua require("harpoon.ui").nav_next()<CR>
" " noremap <silent> <C-k> :lua require("harpoon.ui").nav_prev()<CR>
" nnoremap <leader>a :lua require("harpoon.mark").add_file()<CR>
" nnoremap <silent> <A-e> :lua require("harpoon.ui").nav_file(1)<CR>
" nnoremap <silent> <A-r> :lua require("harpoon.ui").nav_file(2)<CR>
" nnoremap <silent> <A-u> :lua require("harpoon.ui").nav_file(3)<CR>
" nnoremap <silent> <A-i> :lua require("harpoon.ui").nav_file(4)<CR>
" " nnoremap <silent> <A-d> :lua require("harpoon.ui").nav_file(5)<CR>
" " nnoremap <silent> <A-f> :lua require("harpoon.ui").nav_file(6)<CR>
" " nnoremap <silent> <A-j> :lua require("harpoon.ui").nav_file(7)<CR>
" " nnoremap <silent> <A-k> :lua require("harpoon.ui").nav_file(8)<CR>
" " nnoremap <silent> <A-l> :lua require("harpoon.ui").nav_file(9)<CR>
" " nnoremap <silent> <A-5> :lua require("harpoon.ui").nav_file(5)<CR>
" " nnoremap <silent> <A-6> :lua require("harpoon.ui").nav_file(6)<CR>
" " nnoremap <silent> <A-7> :lua require("harpoon.ui").nav_file(7)<CR>
" " nnoremap <silent> <A-8> :lua require("harpoon.ui").nav_file(8)<CR>
" " nnoremap <silent> <A-9> :lua require("harpoon.ui").nav_file(9)<CR>
" harpoon config --------------------





" telescope config --------------------
" Using Lua functions
lua << END
require('telescope').setup {
  defaults = {
    prompt_prefix = " ∮ ",
    -- prompt_prefix = " ⋐⋑ ",
    -- prompt_prefix = " ⊛ ",
    mappings = {
      i = {
        ["<C-j>"] = require('telescope.actions').move_selection_next,
        ["<C-k>"] = require('telescope.actions').move_selection_previous,
        ["<C-n>"] = require('telescope.actions').cycle_history_next,
        ["<C-p>"] = require('telescope.actions').cycle_history_prev,
      },
    },
  },
  -- pickers = {
  --   find_files = {
  --     theme = "dropdown",
  --   }
  -- },
  extensions = {
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    },
    ["ui-select"] = {
      require("telescope.themes").get_dropdown {
        -- winblend = 10,
        -- width = 0.8,
        -- previewer = false,
        -- border = true,
      }
    }
  }
}
require('telescope').load_extension('fzf')
require('telescope').load_extension('ui-select')
-- require('telescope').load_extension('harpoon')
require('telescope').load_extension('zoxide')
-- require("telescope").load_extension("git_worktree")
END
" nnoremap <leader>fa <cmd>Telescope harpoon marks<cr>
" nnoremap <leader>fa <cmd>Telescope harpoon marks<cr>
nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files()<cr>
nnoremap <leader>fg <cmd>lua require('telescope.builtin').live_grep()<cr>
nnoremap <leader>fs <cmd>lua require('telescope.builtin').grep_string()<cr>
nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<cr>
nnoremap <leader>gf <cmd>lua require('telescope.builtin').git_files()<cr>
nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<cr>
nnoremap <leader>fz <cmd>lua require('telescope').extensions.zoxide.list{}<cr>
" telescope config --------------------





" nvim-treesitter config --------------------
" function DisableSyntaxTreesitter()
"     if exists(':TSBufDisable')
"         exec 'TSBufDisable autotag'
"         exec 'TSBufDisable highlight'
"         exec 'TSBufDisable incremental_selection'
"         exec 'TSBufDisable indent'
"         exec 'TSBufDisable playground'
"         exec 'TSBufDisable query_linter'
"         exec 'TSBufDisable rainbow'
"         exec 'TSBufDisable refactor.highlight_definitions'
"         exec 'TSBufDisable refactor.navigation'
"         exec 'TSBufDisable refactor.smart_rename'
"         exec 'TSBufDisable refactor.highlight_current_scope'
"         exec 'TSBufDisable textobjects.swap'
"         " exec 'TSBufDisable textobjects.move'
"         exec 'TSBufDisable textobjects.lsp_interop'
"         exec 'TSBufDisable textobjects.select'
"     endif

"     set foldmethod=manual
" endfunction

" augroup BigFileDisable
"     autocmd!
"     autocmd BufReadPre,FileReadPre * if getfsize(expand("%")) > 200 | exec DisableSyntaxTreesitter() | endif
" augroup END

" function DisableSyntaxTreesitter()
"     if exists(':TSBufDisable')
"         exec 'TSBufDisable autotag'
"         exec 'TSBufDisable highlight'
"         exec 'TSBufDisable incremental_selection'
"         exec 'TSBufDisable indent'
"         exec 'TSBufDisable playground'
"         exec 'TSBufDisable query_linter'
"         exec 'TSBufDisable rainbow'
"         exec 'TSBufDisable refactor.highlight_definitions'
"         exec 'TSBufDisable refactor.navigation'
"         exec 'TSBufDisable refactor.smart_rename'
"         exec 'TSBufDisable refactor.highlight_current_scope'
"         exec 'TSBufDisable textobjects.swap'
"         " exec 'TSBufDisable textobjects.move'
"         exec 'TSBufDisable textobjects.lsp_interop'
"         exec 'TSBufDisable textobjects.select'
"     endif

"     set foldmethod=manual
" endfunction

" augroup BigFileDisable
"     autocmd!
"     autocmd BufReadPre,FileReadPre * if getfsize(expand("%")) > 512 * 1024 | exec DisableSyntaxTreesitter() | endif
" augroup END

lua << END
require'nvim-treesitter.configs'.setup {
  -- context_commentstring = {
  --   enable = true,
  --   enable_autocmd = false,
  --   config = {
  --     javascript = {
  --       __default = '// %s',
  --       jsx_element = '{/* %s */}',
  --       jsx_fragment = '{/* %s */}',
  --       jsx_attribute = '// %s',
  --       comment = '// %s'
  --     },
  --     -- typescript = {
  --     --   __default = '// %s',
  --     --   __multiline = '/* %s */',
  --     --   jsx_element = '{/* %s */}',
  --     --   jsx_fragment = '{/* %s */}',
  --     --   jsx_attribute = '// %s',
  --     --   comment = '// %s'
  --     -- },
  --   }
  -- },
  -- A list of parser names, or "all"
  ensure_installed = { "html", "css", "javascript", "typescript", "json", "json5", "tsx", "lua", "rust", "python", "go", "fish", "bash", "vim", "norg", "pkl" },

  -- Install parsers synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- List of parsers to ignore installing (for "all")
  -- ignore_install = { "javascript" },

  highlight = {
    -- `false` will disable the whole extension
    enable = true,
    disable = function(lang, bufnr)
      return vim.api.nvim_buf_line_count(bufnr) > 1000
    end,

    -- NOTE: these are the names of the parsers and not the filetype. (for example if you want to
    -- disable highlighting for the `tex` filetype, you need to include `latex` in this list as this is
    -- the name of the parser)
    -- list of language that will be disabled
    -- disable = { "c", "rust" },

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
  indent = {
    enable = true
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "<C-Space>", -- set to `false` to disable one of the mappings
      node_incremental = "<C-Space>",
      scope_incremental = "false",
      node_decremental = "<C-M>",
      -- scope_incremental = "grc",
      -- node_decremental = "grm",
    },
  },
  playground = {
    enable = true,
    disable = {},
    updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
    persist_queries = false, -- Whether the query persists across vim sessions
    keybindings = {
      toggle_query_editor = 'o',
      toggle_hl_groups = 'i',
      toggle_injected_languages = 't',
      toggle_anonymous_nodes = 'a',
      toggle_language_display = 'I',
      focus_language = 'f',
      unfocus_language = 'F',
      update = 'R',
      goto_node = '<cr>',
      show_help = '?',
    },
  },
  textobjects = {
    select = {
      enable = true,

      -- Automatically jump forward to textobj, similar to targets.vim
      lookahead = true,

      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        -- ["ac"] = "@class.outer",
        -- you can optionally set descriptions to the mappings (used in the desc parameter of nvim_buf_set_keymap
        -- ["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },

        -- Built-in captures.
        ["as"] = "@statement.outer",
      },
      -- You can choose the select mode (default is charwise 'v')
      selection_modes = {
        ['@parameter.outer'] = 'v', -- charwise
        ['@function.outer'] = 'V', -- linewise
        ['@class.outer'] = '<c-v>', -- blockwise
      },
      -- If you set this to `true` (default is `false`) then any textobject is
      -- extended to include preceding xor succeeding whitespace. Succeeding
      -- whitespace has priority in order to act similarly to eg the built-in
      -- `ap`.
      include_surrounding_whitespace = true,
    },
    -- swap = {
    --   enable = true,
    --   swap_next = {
    --     ["<leader>a"] = "@parameter.inner",
    --   },
    --   swap_previous = {
    --     ["<leader>A"] = "@parameter.inner",
    --   },
    -- },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        ["]m"] = "@function.outer",
        ["]]"] = { query = "@class.outer", desc = "Next class start" },
      },
      goto_next_end = {
        ["]M"] = "@function.outer",
        ["]["] = "@class.outer",
      },
      goto_previous_start = {
        ["[m"] = "@function.outer",
        ["[["] = "@class.outer",
      },
      goto_previous_end = {
        ["[M"] = "@function.outer",
        ["[]"] = "@class.outer",
      },
    },
    lsp_interop = {
      enable = true,
      border = 'none',
      peek_definition_code = {
        ["<leader>Df"] = "@function.outer",
        ["<leader>DF"] = "@class.outer",
      },
    },
  },
}

require'treesitter-context'.setup{
  enable = true, -- Enable this plugin (Can be enabled/disabled later via commands)
  max_lines = 0, -- How many lines the window should span. Values <= 0 mean no limit.
  min_window_height = 0, -- Minimum editor window height to enable context. Values <= 0 mean no limit.
  line_numbers = true,
  multiline_threshold = 20, -- Maximum number of lines to collapse for a single context line
  trim_scope = 'outer', -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
  mode = 'cursor',  -- Line used to calculate context. Choices: 'cursor', 'topline'
  -- Separator between context and content. Should be a single character string, like '-'.
  -- When separator is set, the context will only show up when there are at least 2 lines above cursorline.
  separator = nil,
  zindex = 20, -- The Z-index of the context window
  on_attach = nil, -- (fun(buf: integer): boolean) return false to disable attaching
}

-- require'treesitter-context'.setup{
--     enable = true, -- Enable this plugin (Can be enabled/disabled later via commands)
--     max_lines = 0, -- How many lines the window should span. Values <= 0 mean no limit.
--     trim_scope = 'outer', -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
--     patterns = { -- Match patterns for TS nodes. These get wrapped to match at word boundaries.
--         -- For all filetypes
--         -- Note that setting an entry here replaces all other patterns for this entry.
--         -- By setting the 'default' entry below, you can control which nodes you want to
--         -- appear in the context window.
--         default = {
--             'class',
--             'function',
--             'method',
--             'for',
--             'while',
--             'if',
--             'switch',
--             'case',
--         },
--         -- Patterns for specific filetypes
--         -- If a pattern is missing, *open a PR* so everyone can benefit.
--         tex = {
--             'chapter',
--             'section',
--             'subsection',
--             'subsubsection',
--         },
--         rust = {
--             'impl_item',
--             'struct',
--             'enum',
--         },
--         scala = {
--             'object_definition',
--         },
--         vhdl = {
--             'process_statement',
--             'architecture_body',
--             'entity_declaration',
--         },
--         markdown = {
--             'section',
--         },
--         elixir = {
--             'anonymous_function',
--             'arguments',
--             'block',
--             'do_block',
--             'list',
--             'map',
--             'tuple',
--             'quoted_content',
--         },
--         json = {
--             'pair',
--         },
--         yaml = {
--             'block_mapping_pair',
--         },
--     },
--     exact_patterns = {
--         -- Example for a specific filetype with Lua patterns
--         -- Treat patterns.rust as a Lua pattern (i.e "^impl_item$" will
--         -- exactly match "impl_item" only)
--         -- rust = true,
--     },

--     -- [!] The options below are exposed but shouldn't require your attention,
--     --     you can safely ignore them.

--     zindex = 20, -- The Z-index of the context window
--     mode = 'cursor',  -- Line used to calculate context. Choices: 'cursor', 'topline'
--     -- Separator between context and content. Should be a single character string, like '-'.
--     -- When separator is set, the context will only show up when there are at least 2 lines above cursorline.
--     separator = nil,
-- }

END
" nvim-treesitter config --------------------




" nvim-colorizer config --------------------
lua require('colorizer').setup()
" nvim-colorizer config --------------------






" " vim-expand-region config -----------------
" map <C-Space> <Plug>(expand_region_expand)
" map <C-m> <Plug>(expand_region_shrink)
" 
" " Default settings. (NOTE: Remove comments in dictionary before sourcing)
" let g:expand_region_text_objects = {
"       \ 'iw'  :0,
"       \ 'iW'  :0,
"       \ 'i"'  :0,
"       \ 'i''' :0,
"       \ 'i]'  :1,
"       \ 'ib'  :1,
"       \ 'iB'  :1,
"       \ 'il'  :0,
"       \ 'ip'  :0,
"       \ 'ie'  :0,
"       \ }
" 
" " Extend the global default (NOTE: Remove comments in dictionary before sourcing)
" call expand_region#custom_text_objects({
"       \ "\/\\n\\n\<CR>": 1,
"       \ 'a]' :1,
"       \ 'ab' :1,
"       \ 'aB' :1,
"       \ 'ii' :0,
"       \ 'ai' :0,
"       \ })
" " vim-expand-region config -----------------






" " wildfire.vim config ----------------------
" " This selects the next closest text object.
" map <C-Space> <Plug>(wildfire-fuel)
"
" " This selects the previous closest text object.
" map <C-m> <Plug>(wildfire-water)
"
" let g:wildfire_objects = {
"     \ "*" : ["i'", 'i"', "i)", "i]", "i}", "ip", "it"]
" \ }
"
" cal wildfire#triggers#Add("<ENTER>", {
"     \ "html,xml" : ["at", "it"],
" \ })
"
" " wildfire.vim config ----------------------






" Comment.nvim config --------------------
lua << EOF
require('Comment').setup({
    pre_hook = require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook(),
    ---@param ctx Ctx
    -- pre_hook = function(ctx)
    --     -- Only calculate commentstring for tsx filetypes
    --     if vim.bo.filetype == 'typescriptreact' then
    --         local U = require('Comment.utils')

    --         -- Determine whether to use linewise or blockwise commentstring
    --         local type = ctx.ctype == U.ctype.linewise and '__default' or '__multiline'

    --         -- Determine the location where to calculate commentstring from
    --         local location = nil
    --         if ctx.ctype == U.ctype.blockwise then
    --             location = require('ts_context_commentstring.utils').get_cursor_location()
    --         elseif ctx.cmotion == U.cmotion.v or ctx.cmotion == U.cmotion.V then
    --             location = require('ts_context_commentstring.utils').get_visual_start_location()
    --         end

    --         return require('ts_context_commentstring.internal').calculate_commentstring({
    --             key = type,
    --             location = location,
    --         })
    --     end
    -- end,
    ---@param ctx Ctx
    post_hook = function(ctx)
        if ctx.range.srow == ctx.range.erow then
            -- do something with the current line
        else
            -- do something with lines range
        end
    end,
    -- ignores empty lines
    ignore = '^$',
})
EOF
" Comment.nvim config --------------------




" LuaSnip config --------------------
lua require('plugin/luasnip')
" LuaSnip config --------------------




" " " fern config --------------------
" nnoremap <silent> <leader>ee :<C-u>Fern <C-r>=<SID>smart_path()<CR><CR>
" nnoremap <leader>m :Fern . -reveal=%<CR>
" nnoremap <leader>n :Fern . -drawer -reveal=% -width=46<CR>
" 
" " Set "nerdfont" to g:fern#renderer
" let g:fern#renderer = "nerdfont"
" " let g:fern#renderer = "devicons"
" 
" function! s:smart_path() abort
"   if !empty(&buftype) || bufname('%') =~# '^[^:]\+://'
"     return fnamemodify('.', ':p')
"   endif
"   return fnamemodify(expand('%'), ':p:h')
" endfunction
" 
" " FixCursorHold config
" " in millisecond, used for both CursorHold and CursorHoldI,
" " use updatetime instead if not defined
" let g:cursorhold_updatetime = 100
" 
" function! s:init_fern() abort
"   " Use 'select' instead of 'edit' for default 'open' action
"   nmap <buffer> <Plug>(fern-action-open) <Plug>(fern-action-open:select)
" endfunction
" 
" augroup fern-custom
"   autocmd! *
"   autocmd FileType fern setlocal norelativenumber | setlocal nonumber | call s:init_fern()
" augroup END
" 
" " fern-mapping-fzf.vim config --------------------
" function! Fern_mapping_fzf_customize_option(spec)
"     let a:spec.options .= ' --multi'
"     " Note that fzf#vim#with_preview comes from fzf.vim
"     if exists('*fzf#vim#with_preview')
"         return fzf#vim#with_preview(a:spec)
"     else
"         return a:spec
"     endif
" endfunction
" 
" function! Fern_mapping_fzf_before_all(dict)
"     if !len(a:dict.lines)
"         return
"     endif
"     return a:dict.fern_helper.async.update_marks([])
" endfunction
" 
" function! s:reveal(dict)
"     execute "FernReveal -wait" a:dict.relative_path
"     execute "normal \<Plug>(fern-action-mark:set)"
" endfunction
" " fern-mapping-fzf.vim config --------------------
" 
" 
" function! s:fern_settings() abort
"   nmap <silent> <buffer> p     <Plug>(fern-action-preview:toggle)
"   " nmap <silent> <buffer> <C-p> <Plug>(fern-action-preview:auto:toggle)
"   nmap <silent> <buffer> <C-d> <Plug>(fern-action-preview:scroll:down:half)
"   nmap <silent> <buffer> <C-u> <Plug>(fern-action-preview:scroll:up:half)
" endfunction
" 
" " function! s:fern_settings() abort
" "   nmap <silent> <buffer> <expr> <Plug>(fern-quit-or-close-preview) fern_preview#smart_preview("\<Plug>(fern-action-preview:close)", ":q\<CR>")
" "   nmap <silent> <buffer> q <Plug>(fern-quit-or-close-preview)
" " endfunction
" 
" augroup fern-settings
"   autocmd!
"   autocmd FileType fern call s:fern_settings()
" augroup END
" 
" " augroup FernGroup
" "   autocmd!
" "   autocmd FileType fern setlocal norelativenumber | set nonumber | call s:init_fern()
" " augroup END
" " " fern config --------------------





" nnn.vim config -----------------------
" Disable default mappings
let g:nnn#set_default_mappings = 0

" Set custom mappings
nnoremap <silent> <leader>m :NnnPicker<CR>

" Start n³ in the current file's directory
nnoremap <leader>n :NnnPicker %:p:h<CR>

let g:nnn#action = {
      \ '<c-x>': 'split',
      \ '<c-v>': 'vsplit' }
" nnn.vim config -----------------------





" " lir.nvim config --------------------
" lua << EOF
" local actions = require'lir.actions'
" local mark_actions = require'lir.mark.actions'
" local clipboard_actions = require'lir.clipboard.actions'

" require'lir'.setup {
"   show_hidden_files = false,
"   devicons_enable = true,
"   mappings = {
"     ['l']     = actions.edit,
"     ['<C-s>'] = actions.split,
"     ['<C-v>'] = actions.vsplit,
"     ['<C-t>'] = actions.tabedit,

"     ['h']     = actions.up,
"     ['q']     = actions.quit,

"     ['K']     = actions.mkdir,
"     ['N']     = actions.newfile,
"     ['R']     = actions.rename,
"     ['@']     = actions.cd,
"     ['Y']     = actions.yank_path,
"     ['.']     = actions.toggle_show_hidden,
"     ['D']     = actions.delete,

"     ['J']     = function()
"       mark_actions.toggle_mark()
"       vim.cmd('normal! j')
"     end,
"     ['C']     = clipboard_actions.copy,
"     ['X']     = clipboard_actions.cut,
"     ['P']     = clipboard_actions.paste,
"   },
"   float = {
"     winblend = 0,
"     curdir_window = {
"       enable = false,
"       hightlight_dirname = false
"     },
"   
"     -- -- You can define a function that returns a table to be passed as the third
"     -- -- argument of nvim_open_win().
"     -- win_opts = function()
"     --   local width = math.floor(vim.o.columns * 0.8)
"     --   local height = math.floor(vim.o.lines * 0.8)
"     --   return {
"     --     border = {
"     --       "+", "─", "+", "│", "+", "─", "+", "│",
"     --     },
"     --     width = width,
"     --     height = height,
"     --     row = 1,
"     --     col = math.floor((vim.o.columns - width) / 2),
"     --   }
"     -- end,
"   },
"   hide_cursor = false,
"   on_init = function()
"     -- use visual mode
"     vim.api.nvim_buf_set_keymap(
"       0,
"       "x",
"       "J",
"       ':<C-u>lua require"lir.mark.actions".toggle_mark("v")<CR>',
"       { noremap = true, silent = true }
"     )

"     -- echo cwd
"     vim.api.nvim_echo({ { vim.fn.expand("%:p"), "Normal" } }, false, {})
"   end,
" }

" -- custom folder icon
" require'nvim-web-devicons'.set_icon({
"   lir_folder_icon = {
"     icon = "",
"     color = "#7ebae4",
"     name = "LirFolderNode"
"   }
" })
" EOF
" " lir.nvim config --------------------





" nvim-web-devicons config --------------------
lua << EOF
require'nvim-web-devicons'.setup {
 -- your personnal icons can go here (to override)
 -- you can specify color or cterm_color instead of specifying both of them
 -- DevIcon will be appended to `name`
 override = {
  zsh = {
    icon = "",
    color = "#428850",
    cterm_color = "65",
    name = "Zsh"
  }
 };
 -- globally enable different highlight colors per icon (default to true)
 -- if set to false all icons will have the default icon's color
 color_icons = true;
 -- globally enable default icons (default to false)
 -- will get overriden by `get_icons` option
 default = true;
 -- globally enable "strict" selection of icons - icon will be looked up in
 -- different tables, first by filename, and if not found by extension; this
 -- prevents cases when file doesn't have any extension but still gets some icon
 -- because its name happened to match some extension (default to false)
 strict = true;
 -- same as `override` but specifically for overrides by filename
 -- takes effect when `strict` is true
 override_by_filename = {
  [".gitignore"] = {
    icon = "",
    color = "#f1502f",
    name = "Gitignore"
  }
 };
 -- same as `override` but specifically for overrides by extension
 -- takes effect when `strict` is true
 override_by_extension = {
  ["log"] = {
    icon = "",
    color = "#81e043",
    name = "Log"
  }
 };
}
EOF
" nvim-web-devicons config --------------------





" vim-floaterm config --------------------
" nnoremap <leader>tt :FloatermToggle<CR>
" nnoremap <leader>tn :FloatermNew --height=0.8 --width=0.8 --wintype=float --name=floaterm1 --position=center --autoclose=2<CR>
" augroup TerminalBehavior
"   autocmd!
"   autocmd TermOpen * setlocal listchars= nonumber norelativenumber nowrap winfixwidth scrolloff=0 noruler signcolumn=no
"   autocmd TermOpen * startinsert
"   autocmd TermClose * set showmode ruler scrolloff=3
" augroup END
" augroup Floaterm
"   autocmd! *
"   autocmd FileType floaterm setlocal norelativenumber | setlocal nonumber | setlocal signcolumn=no | setlocal noruler | setlocal scrolloff=0
" augroup END
" vim-floaterm config --------------------





" toggle-lsp-diagnostics.nvim config --------------------
lua <<EOF
require'toggle_lsp_diagnostics'.init()
EOF
" toggle-lsp-diagnostics.nvim config --------------------





" neogit.nvim config --------------------
" lua <<EOF
" local neogit = require("neogit")

" neogit.setup {
"   disable_signs = false,
"   disable_hint = false,
"   disable_context_highlighting = false,
"   disable_commit_confirmation = false,
"   -- Neogit refreshes its internal state after specific events, which can be expensive depending on the repository size. 
"   -- Disabling `auto_refresh` will make it so you have to manually refresh the status after you open it.
"   auto_refresh = true,
"   disable_builtin_notifications = false,
"   use_magit_keybindings = false,
"   commit_popup = {
"       kind = "split",
"   },
"   -- Change the default way of opening neogit
"   kind = "tab",
"   -- customize displayed signs
"   signs = {
"     -- { CLOSED, OPENED }
"     section = { ">", "v" },
"     item = { ">", "v" },
"     hunk = { "", "" },
"   },
"   integrations = {
"     -- Neogit only provides inline diffs. If you want a more traditional way to look at diffs, you can use `sindrets/diffview.nvim`.
"     -- The diffview integration enables the diff popup, which is a wrapper around `sindrets/diffview.nvim`.
"     --
"     -- Requires you to have `sindrets/diffview.nvim` installed.
"     -- use { 
"     --   'TimUntersberger/neogit', 
"     --   requires = { 
"     --     'nvim-lua/plenary.nvim',
"     --     'sindrets/diffview.nvim' 
"     --   }
"     -- }
"     --
"     diffview = true  
"   },
"   -- Setting any section to `false` will make the section not render at all
"   sections = {
"     untracked = {
"       folded = false
"     },
"     unstaged = {
"       folded = false
"     },
"     staged = {
"       folded = false
"     },
"     stashes = {
"       folded = true
"     },
"     unpulled = {
"       folded = true
"     },
"     unmerged = {
"       folded = false
"     },
"     recent = {
"       folded = true
"     },
"   },
"   -- override/add mappings
"   mappings = {
"     -- modify status buffer mappings
"     status = {
"       -- Adds a mapping with "B" as key that does the "BranchPopup" command
"       ["B"] = "BranchPopup",
"       -- Removes the default mapping of "s"
"       ["s"] = "",
"     }
"   }
" }
" EOF
" neogit.nvim config --------------------





" " nvim-autopairs config --------------------
lua << EOF
require("nvim-autopairs").setup {}
EOF
" " nvim-autopairs config --------------------





" nvim-neorg config --------------------
lua << EOF
require('neorg').setup {
  load = {
    ["core.defaults"] = {},
    ["core.norg.dirman"] = {
      config = {
        workspaces = {
          lead = "~/Dropbox/note/lead",
          dev = "~/Dropbox/note/dev",
          linux = "~/Dropbox/note/linux",
          win = "~/Dropbox/note/win",
          music = "~/Dropbox/note/music",
          free = "~/Dropbox/note/free",
          funny = "~/Dropbox/note/funny",
        }
      }
    }
  }
}
EOF
" nvim-neorg config --------------------





" blamer.nvim config --------------------
nnoremap <leader>gb :BlamerToggle<CR>
" blamer.nvim config --------------------





" diffview.nvim config --------------------
nnoremap <leader>dv :DiffviewOpen<CR>
nnoremap <leader>dc :DiffviewClose<CR>
nnoremap <leader>df :DiffviewFileHistory %<CR>
nnoremap <leader>dh :DiffviewFileHistory<CR>

lua <<EOF
-- Lua
-- local actions = require("diffview.actions")

require("diffview").setup{}

-- require("diffview").setup({
--   diff_binaries = false,    -- Show diffs for binaries
--   enhanced_diff_hl = false, -- See ':h diffview-config-enhanced_diff_hl'
--   git_cmd = { "git" },      -- The git executable followed by default args.
--   hg_cmd = { "hg" },        -- The hg executable followed by default args.
--   use_icons = true,         -- Requires nvim-web-devicons
--   show_help_hints = true,   -- Show hints for how to open the help panel
--   watch_index = true,       -- Update views and index buffers when the git index changes.
--   icons = {                 -- Only applies when use_icons is true.
--     folder_closed = "",
--     folder_open = "",
--   },
--   signs = {
--     fold_closed = "",
--     fold_open = "",
--     done = "✓",
--   },
--   view = {
--     -- Configure the layout and behavior of different types of views.
--     -- Available layouts:
--     --  'diff1_plain'
--     --    |'diff2_horizontal'
--     --    |'diff2_vertical'
--     --    |'diff3_horizontal'
--     --    |'diff3_vertical'
--     --    |'diff3_mixed'
--     --    |'diff4_mixed'
--     -- For more info, see ':h diffview-config-view.x.layout'.
--     default = {
--       -- Config for changed files, and staged files in diff views.
--       layout = "diff2_horizontal",
--       winbar_info = false,          -- See ':h diffview-config-view.x.winbar_info'
--     },
--     merge_tool = {
--       -- Config for conflicted files in diff views during a merge or rebase.
--       layout = "diff3_horizontal",
--       disable_diagnostics = true,   -- Temporarily disable diagnostics for conflict buffers while in the view.
--       winbar_info = true,           -- See ':h diffview-config-view.x.winbar_info'
--     },
--     file_history = {
--       -- Config for changed files in file history views.
--       layout = "diff2_horizontal",
--       winbar_info = false,          -- See ':h diffview-config-view.x.winbar_info'
--     },
--   },
--   file_panel = {
--     listing_style = "tree",             -- One of 'list' or 'tree'
--     tree_options = {                    -- Only applies when listing_style is 'tree'
--       flatten_dirs = true,              -- Flatten dirs that only contain one single dir
--       folder_statuses = "only_folded",  -- One of 'never', 'only_folded' or 'always'.
--     },
--     win_config = {                      -- See ':h diffview-config-win_config'
--       position = "left",
--       width = 35,
--       win_opts = {}
--     },
--   },
--   file_history_panel = {
--     log_options = {   -- See ':h diffview-config-log_options'
--       git = {
--         single_file = {
--           diff_merges = "combined",
--         },
--         multi_file = {
--           diff_merges = "first-parent",
--         },
--       },
--       hg = {
--         single_file = {},
--         multi_file = {},
--       },
--     },
--     win_config = {    -- See ':h diffview-config-win_config'
--       position = "bottom",
--       height = 16,
--       win_opts = {}
--     },
--   },
--   commit_log_panel = {
--     win_config = {   -- See ':h diffview-config-win_config'
--       win_opts = {},
--     }
--   },
--   default_args = {    -- Default args prepended to the arg-list for the listed commands
--     DiffviewOpen = {},
--     DiffviewFileHistory = {},
--   },
--   hooks = {},         -- See ':h diffview-config-hooks'
--   keymaps = {
--     disable_defaults = false, -- Disable the default keymaps
--     view = {
--       -- The `view` bindings are active in the diff buffers, only when the current
--       -- tabpage is a Diffview.
--       { "n", "<tab>",       actions.select_next_entry,              { desc = "Open the diff for the next file" } },
--       { "n", "<s-tab>",     actions.select_prev_entry,              { desc = "Open the diff for the previous file" } },
--       { "n", "gf",          actions.goto_file_edit,                 { desc = "Open the file in the previous tabpage" } },
--       { "n", "<C-w><C-f>",  actions.goto_file_split,                { desc = "Open the file in a new split" } },
--       { "n", "<C-w>gf",     actions.goto_file_tab,                  { desc = "Open the file in a new tabpage" } },
--       { "n", "<leader>e",   actions.focus_files,                    { desc = "Bring focus to the file panel" } },
--       { "n", "<leader>b",   actions.toggle_files,                   { desc = "Toggle the file panel." } },
--       { "n", "g<C-x>",      actions.cycle_layout,                   { desc = "Cycle through available layouts." } },
--       { "n", "[x",          actions.prev_conflict,                  { desc = "In the merge-tool: jump to the previous conflict" } },
--       { "n", "]x",          actions.next_conflict,                  { desc = "In the merge-tool: jump to the next conflict" } },
--       { "n", "<leader>co",  actions.conflict_choose("ours"),        { desc = "Choose the OURS version of a conflict" } },
--       { "n", "<leader>ct",  actions.conflict_choose("theirs"),      { desc = "Choose the THEIRS version of a conflict" } },
--       { "n", "<leader>cb",  actions.conflict_choose("base"),        { desc = "Choose the BASE version of a conflict" } },
--       { "n", "<leader>ca",  actions.conflict_choose("all"),         { desc = "Choose all the versions of a conflict" } },
--       { "n", "dx",          actions.conflict_choose("none"),        { desc = "Delete the conflict region" } },
--       { "n", "<leader>cO",  actions.conflict_choose_all("ours"),    { desc = "Choose the OURS version of a conflict for the whole file" } },
--       { "n", "<leader>cT",  actions.conflict_choose_all("theirs"),  { desc = "Choose the THEIRS version of a conflict for the whole file" } },
--       { "n", "<leader>cB",  actions.conflict_choose_all("base"),    { desc = "Choose the BASE version of a conflict for the whole file" } },
--       { "n", "<leader>cA",  actions.conflict_choose_all("all"),     { desc = "Choose all the versions of a conflict for the whole file" } },
--       { "n", "dX",          actions.conflict_choose_all("none"),    { desc = "Delete the conflict region for the whole file" } },
--     },
--     diff1 = {
--       -- Mappings in single window diff layouts
--       { "n", "g?", actions.help({ "view", "diff1" }), { desc = "Open the help panel" } },
--     },
--     diff2 = {
--       -- Mappings in 2-way diff layouts
--       { "n", "g?", actions.help({ "view", "diff2" }), { desc = "Open the help panel" } },
--     },
--     diff3 = {
--       -- Mappings in 3-way diff layouts
--       { { "n", "x" }, "2do",  actions.diffget("ours"),            { desc = "Obtain the diff hunk from the OURS version of the file" } },
--       { { "n", "x" }, "3do",  actions.diffget("theirs"),          { desc = "Obtain the diff hunk from the THEIRS version of the file" } },
--       { "n",          "g?",   actions.help({ "view", "diff3" }),  { desc = "Open the help panel" } },
--     },
--     diff4 = {
--       -- Mappings in 4-way diff layouts
--       { { "n", "x" }, "1do",  actions.diffget("base"),            { desc = "Obtain the diff hunk from the BASE version of the file" } },
--       { { "n", "x" }, "2do",  actions.diffget("ours"),            { desc = "Obtain the diff hunk from the OURS version of the file" } },
--       { { "n", "x" }, "3do",  actions.diffget("theirs"),          { desc = "Obtain the diff hunk from the THEIRS version of the file" } },
--       { "n",          "g?",   actions.help({ "view", "diff4" }),  { desc = "Open the help panel" } },
--     },
--     file_panel = {
--       { "n", "j",              actions.next_entry,                     { desc = "Bring the cursor to the next file entry" } },
--       { "n", "<down>",         actions.next_entry,                     { desc = "Bring the cursor to the next file entry" } },
--       { "n", "k",              actions.prev_entry,                     { desc = "Bring the cursor to the previous file entry" } },
--       { "n", "<up>",           actions.prev_entry,                     { desc = "Bring the cursor to the previous file entry" } },
--       { "n", "<cr>",           actions.select_entry,                   { desc = "Open the diff for the selected entry" } },
--       { "n", "o",              actions.select_entry,                   { desc = "Open the diff for the selected entry" } },
--       { "n", "l",              actions.select_entry,                   { desc = "Open the diff for the selected entry" } },
--       { "n", "<2-LeftMouse>",  actions.select_entry,                   { desc = "Open the diff for the selected entry" } },
--       { "n", "-",              actions.toggle_stage_entry,             { desc = "Stage / unstage the selected entry" } },
--       { "n", "S",              actions.stage_all,                      { desc = "Stage all entries" } },
--       { "n", "U",              actions.unstage_all,                    { desc = "Unstage all entries" } },
--       { "n", "X",              actions.restore_entry,                  { desc = "Restore entry to the state on the left side" } },
--       { "n", "L",              actions.open_commit_log,                { desc = "Open the commit log panel" } },
--       { "n", "zo",             actions.open_fold,                      { desc = "Expand fold" } },
--       { "n", "h",              actions.close_fold,                     { desc = "Collapse fold" } },
--       { "n", "zc",             actions.close_fold,                     { desc = "Collapse fold" } },
--       { "n", "za",             actions.toggle_fold,                    { desc = "Toggle fold" } },
--       { "n", "zR",             actions.open_all_folds,                 { desc = "Expand all folds" } },
--       { "n", "zM",             actions.close_all_folds,                { desc = "Collapse all folds" } },
--       { "n", "<c-b>",          actions.scroll_view(-0.25),             { desc = "Scroll the view up" } },
--       { "n", "<c-f>",          actions.scroll_view(0.25),              { desc = "Scroll the view down" } },
--       { "n", "<tab>",          actions.select_next_entry,              { desc = "Open the diff for the next file" } },
--       { "n", "<s-tab>",        actions.select_prev_entry,              { desc = "Open the diff for the previous file" } },
--       { "n", "gf",             actions.goto_file_edit,                 { desc = "Open the file in the previous tabpage" } },
--       { "n", "<C-w><C-f>",     actions.goto_file_split,                { desc = "Open the file in a new split" } },
--       { "n", "<C-w>gf",        actions.goto_file_tab,                  { desc = "Open the file in a new tabpage" } },
--       { "n", "i",              actions.listing_style,                  { desc = "Toggle between 'list' and 'tree' views" } },
--       { "n", "f",              actions.toggle_flatten_dirs,            { desc = "Flatten empty subdirectories in tree listing style" } },
--       { "n", "R",              actions.refresh_files,                  { desc = "Update stats and entries in the file list" } },
--       { "n", "<leader>e",      actions.focus_files,                    { desc = "Bring focus to the file panel" } },
--       { "n", "<leader>b",      actions.toggle_files,                   { desc = "Toggle the file panel" } },
--       { "n", "g<C-x>",         actions.cycle_layout,                   { desc = "Cycle available layouts" } },
--       { "n", "[x",             actions.prev_conflict,                  { desc = "Go to the previous conflict" } },
--       { "n", "]x",             actions.next_conflict,                  { desc = "Go to the next conflict" } },
--       { "n", "g?",             actions.help("file_panel"),             { desc = "Open the help panel" } },
--       { "n", "<leader>cO",     actions.conflict_choose_all("ours"),    { desc = "Choose the OURS version of a conflict for the whole file" } },
--       { "n", "<leader>cT",     actions.conflict_choose_all("theirs"),  { desc = "Choose the THEIRS version of a conflict for the whole file" } },
--       { "n", "<leader>cB",     actions.conflict_choose_all("base"),    { desc = "Choose the BASE version of a conflict for the whole file" } },
--       { "n", "<leader>cA",     actions.conflict_choose_all("all"),     { desc = "Choose all the versions of a conflict for the whole file" } },
--       { "n", "dX",             actions.conflict_choose_all("none"),    { desc = "Delete the conflict region for the whole file" } },
--     },
--     file_history_panel = {
--       { "n", "g!",            actions.options,                     { desc = "Open the option panel" } },
--       { "n", "<C-A-d>",       actions.open_in_diffview,            { desc = "Open the entry under the cursor in a diffview" } },
--       { "n", "y",             actions.copy_hash,                   { desc = "Copy the commit hash of the entry under the cursor" } },
--       { "n", "L",             actions.open_commit_log,             { desc = "Show commit details" } },
--       { "n", "zR",            actions.open_all_folds,              { desc = "Expand all folds" } },
--       { "n", "zM",            actions.close_all_folds,             { desc = "Collapse all folds" } },
--       { "n", "j",             actions.next_entry,                  { desc = "Bring the cursor to the next file entry" } },
--       { "n", "<down>",        actions.next_entry,                  { desc = "Bring the cursor to the next file entry" } },
--       { "n", "k",             actions.prev_entry,                  { desc = "Bring the cursor to the previous file entry." } },
--       { "n", "<up>",          actions.prev_entry,                  { desc = "Bring the cursor to the previous file entry." } },
--       { "n", "<cr>",          actions.select_entry,                { desc = "Open the diff for the selected entry." } },
--       { "n", "o",             actions.select_entry,                { desc = "Open the diff for the selected entry." } },
--       { "n", "<2-LeftMouse>", actions.select_entry,                { desc = "Open the diff for the selected entry." } },
--       { "n", "<c-b>",         actions.scroll_view(-0.25),          { desc = "Scroll the view up" } },
--       { "n", "<c-f>",         actions.scroll_view(0.25),           { desc = "Scroll the view down" } },
--       { "n", "<tab>",         actions.select_next_entry,           { desc = "Open the diff for the next file" } },
--       { "n", "<s-tab>",       actions.select_prev_entry,           { desc = "Open the diff for the previous file" } },
--       { "n", "gf",            actions.goto_file_edit,              { desc = "Open the file in the previous tabpage" } },
--       { "n", "<C-w><C-f>",    actions.goto_file_split,             { desc = "Open the file in a new split" } },
--       { "n", "<C-w>gf",       actions.goto_file_tab,               { desc = "Open the file in a new tabpage" } },
--       { "n", "<leader>e",     actions.focus_files,                 { desc = "Bring focus to the file panel" } },
--       { "n", "<leader>b",     actions.toggle_files,                { desc = "Toggle the file panel" } },
--       { "n", "g<C-x>",        actions.cycle_layout,                { desc = "Cycle available layouts" } },
--       { "n", "g?",            actions.help("file_history_panel"),  { desc = "Open the help panel" } },
--     },
--     option_panel = {
--       { "n", "<tab>", actions.select_entry,          { desc = "Change the current option" } },
--       { "n", "q",     actions.close,                 { desc = "Close the panel" } },
--       { "n", "g?",    actions.help("option_panel"),  { desc = "Open the help panel" } },
--     },
--     help_panel = {
--       { "n", "q",     actions.close,  { desc = "Close help menu" } },
--       { "n", "<esc>", actions.close,  { desc = "Close help menu" } },
--     },
--   },
-- })
EOF
" diffview.nvim config --------------------






" nvim-surround config --------------------
lua <<EOF
require("nvim-surround").setup()
EOF
" nvim-surround config --------------------






" nvim-bqf config --------------------
lua <<EOF
vim.cmd([[
    hi BqfPreviewBorder guifg=#50a14f ctermfg=71
    hi link BqfPreviewRange Search
]])

require('bqf').setup({
    auto_enable = true,
    auto_resize_height = true, -- highly recommended enable
    preview = {
        win_height = 15,
        win_vheight = 15,
        delay_syntax = 80,
        border_chars = {'┃', '┃', '━', '━', '┏', '┓', '┗', '┛', '█'},
        show_title = true,
        should_preview_cb = function(bufnr, qwinid)
            local ret = true
            local bufname = vim.api.nvim_buf_get_name(bufnr)
            local fsize = vim.fn.getfsize(bufname)
            if fsize > 100 * 1024 then
                -- skip file size greater than 100k
                ret = false
            elseif bufname:match('^fugitive://') then
                -- skip fugitive buffer
                ret = false
            end
            return ret
        end
    },
    -- make `drop` and `tab drop` to become preferred
    func_map = {
        drop = 'o',
        openc = 'O',
        split = '<C-s>',
        tabdrop = '<C-t>',
        tabc = '',
        ptogglemode = 'z,',
    },
    filter = {
        fzf = {
            action_for = {['ctrl-s'] = 'split', ['ctrl-t'] = 'tab drop'},
            extra_opts = {'--bind', 'ctrl-o:toggle-all', '--prompt', '> '}
        }
    }
})
EOF
" nvim-bqf config --------------------






" neovim-session-manager config --------------------
lua <<EOF
local Path = require('plenary.path')
require('session_manager').setup({
  sessions_dir = Path:new(vim.fn.stdpath('data'), 'sessions'), -- The directory where the session files will be saved.
  path_replacer = '__', -- The character to which the path separator will be replaced for session files.
  colon_replacer = '++', -- The character to which the colon symbol will be replaced for session files.
  -- autoload_mode = require('session_manager.config').AutoloadMode.LastSession, -- Define what to do when Neovim is started without arguments. Possible values: Disabled, CurrentDir, LastSession
  autoload_mode = require('session_manager.config').AutoloadMode.Disabled,
  autosave_last_session = true, -- Automatically save last session on exit and on session switch.
  autosave_ignore_not_normal = true, -- Plugin will not save a session when no buffers are opened, or all of them aren't writable or listed.
  autosave_ignore_filetypes = { -- All buffers of these file types will be closed before the session is saved.
    'gitcommit',
  },
  autosave_only_in_session = false, -- Always autosaves session. If true, only autosaves after a session is active.
  max_path_length = 80, -- Shorten the display path if length exceeds this threshold. Use 0 if don't want to shorten the path at all.
})

vim.api.nvim_set_keymap("n", "<Leader>sl", ":SessionManager load_current_dir_session<CR>", { })

EOF
" neovim-session-manager config --------------------






" vim-table-mode config -----------------------
" For Markdown-compatible tables
let g:table_mode_corner='|'
" nnoremap <leader>tb :TableModeToggle<CR>
" vim-table-mode config -----------------------






" vim-grepper config -----------------------
" for browsing the input history
" cnoremap <c-n> <down>
" cnoremap <c-p> <up>

" Grep string with rg
" nnoremap <leader>rg :GrepperRg 

" Grep yanked string
" nnoremap <leader>rs :GrepperRg "<C-r>""<CR>

" nnoremap <leader>rs :execute 'GrepperRg' '<C-r>' '"'<CR>
" vnoremap <leader>rs "+y:GrepperRg "<C-r>""<CR>
vnoremap <leader>rs "+y:Grepper -tool rg<CR><C-r>"<CR>

nnoremap <leader>rg :Grepper -tool rg<cr>
nnoremap <leader>gg :Grepper -tool git<cr>
nnoremap <leader>ga :Grepper -tool ag<cr>
nnoremap <leader>gs :Grepper -tool ag -side<cr>
nnoremap <leader>*  :Grepper -tool ag -cword -noprompt<cr>

let g:grepper = {}
let g:grepper.tools = ['git', 'ag', 'rg', 'grep']
" let g:grepper.open = 0
" let g:grepper.jump = 1
let g:grepper.prompt_mapping_tool = '<leader>g'

command! Todo Grepper -noprompt -tool git -query -E '(TODO|FIXME|XXX):'

" vim-grepper config -----------------------






" neoformat config -----------------------
let g:neoformat_try_node_exe = 1

nnoremap <leader>fx :Neoformat<CR>

" let g:neoformat_typescriptreact_eslint_d = {
"             \ 'exe': 'eslint_d',
"             \ 'args': ['--fix'],
"             \ 'replace': 1,
"             \ 'stdin': 1,
"             \ 'env': ["DEBUG=1"],
"             \ 'valid_exit_codes': [0, 23],
"             \ 'no_append': 1,
"             \ }

let g:neoformat_enabled_javascript      = ['eslint_d']
let g:neoformat_enabled_javascriptreact = ['eslint_d']
let g:neoformat_enabled_typescript      = ['eslint_d']
let g:neoformat_enabled_typescriptreact = ['eslint_d']

" neoformat config -----------------------





" tabnine config ----------------------
" lua <<EOF
" require('tabnine').setup({
"   disable_auto_comment=true, 
"   accept_keymap="<Tab>",
"   dismiss_keymap = "<C-]>",
"   debounce_ms = 800,
"   suggestion_color = {gui = "#808080", cterm = 244},
"   exclude_filetypes = {"TelescopePrompt"}
" })
" EOF
" tabnine config ----------------------




" fidget config ----------------------
lua <<EOF
require"fidget".setup{}
EOF
" fidget config ----------------------





" vimtex config ----------------------

" This is necessary for VimTeX to load properly. The "indent" is optional.
" Note that most plugin managers will do this automatically.
filetype plugin indent on

" This enables Vim's and neovim's syntax-related features. Without this, some
" VimTeX features will not work (see ":help vimtex-requirements" for more
" info).
syntax enable

" Viewer options: One may configure the viewer either by specifying a built-in
" viewer method:
let g:vimtex_view_method = 'zathura'

" Or with a generic interface:
let g:vimtex_view_general_viewer = 'okular'
let g:vimtex_view_general_options = '--unique file:@pdf\#src:@line@tex'

" VimTeX uses latexmk as the default compiler backend. If you use it, which is
" strongly recommended, you probably don't need to configure anything. If you
" want another compiler backend, you can change it as follows. The list of
" supported backends and further explanation is provided in the documentation,
" see ":help vimtex-compiler".
let g:vimtex_compiler_method = 'latexrun'

" Most VimTeX mappings rely on localleader and this can be changed with the
" following line. The default is usually fine and is the symbol "\".
" let maplocalleader = ","

" vimtex config ----------------------





" neural config ----------------------
" lua <<EOF
" -- Configure Neural like so in Lua
" require('neural').setup({
"     source = {
"         openai = {
"             api_key = vim.env.OPENAI_API_KEY,
"         },
"     },
" })
" EOF
" neural config ----------------------





" transparent.nvim config ----------------------
lua <<EOF
-- require("transparent").setup({
--   groups = { -- table: default groups
--     'Normal', 'NormalNC', 'Comment', 'Constant', 'Special', 'Identifier',
--     'Statement', 'PreProc', 'Type', 'Underlined', 'Todo', 'String', 'Function',
--     'Conditional', 'Repeat', 'Operator', 'Structure', 'LineNr', 'NonText',
--     'SignColumn', 'CursorLineNr', 'EndOfBuffer',
--   },
--   extra_groups = {}, -- table: additional groups that should be cleared
--   exclude_groups = {}, -- table: groups you don't want to clear
-- })

vim.api.nvim_set_keymap("n", "yoa", ":TransparentToggle<CR>", { })

EOF
" transparent.nvim config ----------------------





" undotree config ----------------------
nnoremap <leader>u :UndotreeToggle<CR>
" undotree config ----------------------






" " codeium.vim config ----------------------
" let g:codeium_enabled = v:false
" " codeium.vim config ----------------------






" zen-mode.nvim config --------------------
nnoremap <leader>z :ZenMode<cr>
" zen-mode.nvim config --------------------






" " pkl-neovim config -----------------
" " The below is required for enabling the tree-sitter syntax engine, which is used by pkl-neovim.
" lua <<EOF
" local hasConfigs, configs = pcall(require, "nvim-treesitter.configs")
" if hasConfigs then
"   configs.setup {
"     ensure_installed = "pkl",
"     highlight = {
"       enable = true,              -- false will disable the whole extension
"     },
"     indent = {
"       enable = true
"     }
"   }
" end
" EOF
" " pkl-neovim config -----------------


" oil.nvim config -----------------
lua << END
require("oil").setup({
  -- Oil will take over directory buffers (e.g. `vim .` or `:e src/`)
  -- Set to false if you want some other plugin (e.g. netrw) to open when you edit directories.
  default_file_explorer = true,
  -- Id is automatically added at the beginning, and name at the end
  -- See :help oil-columns
  columns = {
    "icon",
    -- "permissions",
    -- "size",
    -- "mtime",
  },
  -- Buffer-local options to use for oil buffers
  buf_options = {
    buflisted = false,
    bufhidden = "hide",
  },
  -- Window-local options to use for oil buffers
  win_options = {
    wrap = false,
    signcolumn = "no",
    cursorcolumn = false,
    foldcolumn = "0",
    spell = false,
    list = false,
    conceallevel = 3,
    concealcursor = "nvic",
  },
  -- Send deleted files to the trash instead of permanently deleting them (:help oil-trash)
  delete_to_trash = false,
  -- Skip the confirmation popup for simple operations (:help oil.skip_confirm_for_simple_edits)
  skip_confirm_for_simple_edits = false,
  -- Selecting a new/moved/renamed file or directory will prompt you to save changes first
  -- (:help prompt_save_on_select_new_entry)
  prompt_save_on_select_new_entry = true,
  -- Oil will automatically delete hidden buffers after this delay
  -- You can set the delay to false to disable cleanup entirely
  -- Note that the cleanup process only starts when none of the oil buffers are currently displayed
  cleanup_delay_ms = 2000,
  lsp_file_methods = {
    -- Enable or disable LSP file operations
    enabled = true,
    -- Time to wait for LSP file operations to complete before skipping
    timeout_ms = 1000,
    -- Set to true to autosave buffers that are updated with LSP willRenameFiles
    -- Set to "unmodified" to only save unmodified buffers
    autosave_changes = false,
  },
  -- Constrain the cursor to the editable parts of the oil buffer
  -- Set to `false` to disable, or "name" to keep it on the file names
  constrain_cursor = "editable",
  -- Set to true to watch the filesystem for changes and reload oil
  watch_for_changes = false,
  -- Keymaps in oil buffer. Can be any value that `vim.keymap.set` accepts OR a table of keymap
  -- options with a `callback` (e.g. { callback = function() ... end, desc = "", mode = "n" })
  -- Additionally, if it is a string that matches "actions.<name>",
  -- it will use the mapping at require("oil.actions").<name>
  -- Set to `false` to remove a keymap
  -- See :help oil-actions for a list of all available actions
  keymaps = {
    ["g?"] = "actions.show_help",
    ["<CR>"] = "actions.select",
    ["<C-s>"] = { "actions.select", opts = { vertical = true }, desc = "Open the entry in a vertical split" },
    ["<C-h>"] = { "actions.select", opts = { horizontal = true }, desc = "Open the entry in a horizontal split" },
    ["<C-t>"] = { "actions.select", opts = { tab = true }, desc = "Open the entry in new tab" },
    ["<C-p>"] = "actions.preview",
    ["<C-c>"] = "actions.close",
    ["<C-l>"] = "actions.refresh",
    ["-"] = "actions.parent",
    ["<C-H>"] = "actions.open_cwd",
    ["`"] = "actions.cd",
    ["~"] = { "actions.cd", opts = { scope = "tab" }, desc = ":tcd to the current oil directory", mode = "n" },
    ["gs"] = "actions.change_sort",
    ["gx"] = "actions.open_external",
    ["g."] = "actions.toggle_hidden",
    ["g\\"] = "actions.toggle_trash",
  },
  -- Set to false to disable all of the above keymaps
  use_default_keymaps = true,
  view_options = {
    -- Show files and directories that start with "."
    show_hidden = false,
    -- This function defines what is considered a "hidden" file
    is_hidden_file = function(name, bufnr)
      local m = name:match("^%.")
      return m ~= nil
    end,
    -- This function defines what will never be shown, even when `show_hidden` is set
    is_always_hidden = function(name, bufnr)
      return false
    end,
    -- Sort file names with numbers in a more intuitive order for humans.
    -- Can be "fast", true, or false. "fast" will turn it off for large directories.
    natural_order = "fast",
    -- Sort file and directory names case insensitive
    case_insensitive = false,
    sort = {
      -- sort order can be "asc" or "desc"
      -- see :help oil-columns to see which columns are sortable
      { "type", "asc" },
      { "name", "asc" },
    },
    -- Customize the highlight group for the file name
    highlight_filename = function(entry, is_hidden, is_link_target, is_link_orphan)
      return nil
    end,
  },
  -- Extra arguments to pass to SCP when moving/copying files over SSH
  extra_scp_args = {},
  -- EXPERIMENTAL support for performing file operations with git
  git = {
    -- Return true to automatically git add/mv/rm files
    add = function(path)
      return false
    end,
    mv = function(src_path, dest_path)
      return false
    end,
    rm = function(path)
      return false
    end,
  },
  -- Configuration for the floating window in oil.open_float
  float = {
    -- Padding around the floating window
    padding = 2,
    max_width = 0,
    max_height = 0,
    border = "rounded",
    win_options = {
      winblend = 0,
    },
    -- optionally override the oil buffers window title with custom function: fun(winid: integer): string
    get_win_title = nil,
    -- preview_split: Split direction: "auto", "left", "right", "above", "below".
    preview_split = "auto",
    -- This is the config that will be passed to nvim_open_win.
    -- Change values here to customize the layout
    override = function(conf)
      return conf
    end,
  },
  -- Configuration for the file preview window
  preview_win = {
    -- Whether the preview window is automatically updated when the cursor is moved
    update_on_cursor_moved = true,
    -- How to open the preview window "load"|"scratch"|"fast_scratch"
    preview_method = "fast_scratch",
    -- A function that returns true to disable preview on a file e.g. to avoid lag
    disable_preview = function(filename)
      return false
    end,
    -- Window-local options to use for preview window buffers
    win_options = {},
  },
  -- Configuration for the floating action confirmation window
  confirmation = {
    -- Width dimensions can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
    -- min_width and max_width can be a single value or a list of mixed integer/float types.
    -- max_width = {100, 0.8} means "the lesser of 100 columns or 80% of total"
    max_width = 0.9,
    -- min_width = {40, 0.4} means "the greater of 40 columns or 40% of total"
    min_width = { 40, 0.4 },
    -- optionally define an integer/float for the exact width of the preview window
    width = nil,
    -- Height dimensions can be integers or a float between 0 and 1 (e.g. 0.4 for 40%)
    -- min_height and max_height can be a single value or a list of mixed integer/float types.
    -- max_height = {80, 0.9} means "the lesser of 80 columns or 90% of total"
    max_height = 0.9,
    -- min_height = {5, 0.1} means "the greater of 5 columns or 10% of total"
    min_height = { 5, 0.1 },
    -- optionally define an integer/float for the exact height of the preview window
    height = nil,
    border = "rounded",
    win_options = {
      winblend = 0,
    },
  },
  -- Configuration for the floating progress window
  progress = {
    max_width = 0.9,
    min_width = { 40, 0.4 },
    width = nil,
    max_height = { 10, 0.9 },
    min_height = { 5, 0.1 },
    height = nil,
    border = "rounded",
    minimized_border = "none",
    win_options = {
      winblend = 0,
    },
  },
  -- Configuration for the floating SSH window
  ssh = {
    border = "rounded",
  },
  -- Configuration for the floating keymaps help window
  keymaps_help = {
    border = "rounded",
  },
})

-- vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })
vim.keymap.set("n", "<leader>e", "<CMD>Oil<CR>", { desc = "Open parent directory" })
END
" oil.nvim config -----------------
