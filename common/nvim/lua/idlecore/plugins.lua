local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

local plugins = {
    {
        "scrooloose/nerdtree",
    },
    {
        'catppuccin/nvim',
        name = 'catppuccin',
        priority = 1000
    },
    {
        "vim-airline/vim-airline"
    },
    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        config = function ()
            local configs = require("nvim-treesitter.configs")

            configs.setup({
                ensure_installed = {"c", "cpp", "lua", "vim", "vimdoc", "javascript", "typescript", "html"},
                sync_install = true,
                highlight = { enable = true }
            })
        end
    },
    {
        "tpope/vim-fugitive"
    },
    {
        "VonHeikemen/lsp-zero.nvim", branch = "v3.x",
        dependencies = {
            "neovim/nvim-lspconfig",
            'hrsh7th/cmp-nvim-lsp',
            'hrsh7th/nvim-cmp',
            'L3MON4D3/LuaSnip',
            'williamboman/mason.nvim',
            'williamboman/mason-lspconfig.nvim',
        }
    },
    {
        'folke/neodev.nvim', opts={}
    },
    {
        'nvim-telescope/telescope.nvim', tag='0.1.5',
        dependencies = {
            'nvim-lua/plenary.nvim',
            'BurntSushi/ripgrep',
        },
    },
    {
        'windwp/nvim-autopairs',
        event = "InsertEnter",
        opts = {}
    },
    {
        "Olical/conjure"
    },
    {
        "HiPhish/rainbow-delimiters.nvim"
    },
    {
        "github/copilot.vim"
    },
    {
        "CopilotC-Nvim/CopilotChat.nvim",
        dependencies = {
            {"github/copilot.vim"},
            {"nvim-lua/plenary.nvim", branch = "master"}
        },
        opts = {
            mappings = {
                close = {
                    normal = 'q',
                    insert = ''
                },
                show_diff = {
                    full_diff = true
                }
            }
        }
    }
}

require("lazy").setup(plugins)

local actions = require("telescope.actions")
require('telescope').setup{
    defaults = {
        mappings = {
            i = {
                ["<C-c>"] = actions.close,
                ["<C-s>"] = actions.file_vsplit
            }
        },
        --path_display = {"truncate"},
        layout_strategy = 'vertical',
    },
    pickers = {
    },
    extensions = {
    }
}

