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
-- I need this to do Svelte development on PowerShell
if idlecore_common.isWindows then
    vim.opt.shellslash = true
end

vim.diagnostic.config({
    virtual_text = false,
    virtual_lines = false,
    float = {
        border = "rounded",
        focusable = false,
        --style = "minimal",
        --source = "always",
        header = "",
        prefix = "",
        --anchor = "SE",
    },
})

vim.cmd [[
    autocmd CursorMoved * lua vim.diagnostic.open_float(nil, { focus = false })
]]
