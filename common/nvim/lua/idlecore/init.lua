local idlecore_common = require("idlecore.common")

vim.g.mapleader = ","
vim.g.maplocalleader = ","


require('idlecore.plugins')
require('idlecore.mapping')
require('idlecore.lsp')
require('idlecore.autocmds')


-- Colors
vim.cmd("colorscheme catppuccin-mocha")
vim.cmd("AirlineTheme catppuccin")


-- 4 Space tabs
vim.opt.tabstop = 4
vim.opt.softtabstop = 0
vim.opt.shiftwidth = 0
vim.opt.expandtab = true
vim.opt.autoindent = true
vim.opt.smarttab = false

vim.opt.colorcolumn = '80'

vim.opt.incsearch = true

vim.opt.number = true
vim.opt.relativenumber = true

vim.opt.guifont = "Consolas:h11"

-- ----------------------------------------------------------------------------
--                                     WARNING
-- ----------------------------------------------------------------------------
-- If something is broken, it's probably this line.
-- I need this to do svelte development on Powershell
if idlecore_common.isWindows then
    vim.opt.shellslash = true
end
