local function noremap(mode, keypress, output)
    vim.api.nvim_set_keymap(mode, keypress, output, {noremap = true})
end

noremap('n', ':', ';')
noremap('n', ';', ':')
noremap('v', ':', ';')
noremap('v', ';', ':')

-- Fix insert mode behavior
noremap('i', '<C-c>', '<Esc>')
noremap('i', '<S-Tab>', '<C-d>')


-- Toggle nerd tree
noremap('n', '<Leader>n', ':NERDTreeToggle<CR>')

-- Toggle Vim Fugitive
noremap('n', '<Leader>g', ':G<CR><C-w><S-h><C-w>30<')
noremap('n', '<Leader>gb', ':G blame<CR>')

-- Find and Replace
noremap('n', '<Leader>r', ':%s/')

-- Open mason
noremap('n', '<Leader>l', ':Mason<CR>')

-- Set dir to current file dir
noremap('n', '<Leader>cd', ':cd %:p:h<CR>')

------------------------------------------------------------------------------
-- Telescope
------------------------------------------------------------------------------
local builtin = require('telescope.builtin')
vim.keymap.set('n', '<Leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<Leader>fp', builtin.git_files, {})
vim.keymap.set('n', '<Leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<Leader>fr', function () builtin.lsp_references({fname_width=100}) end, {})

------------------------------------------------------------------------------
-- Build scripts
------------------------------------------------------------------------------
local mainfile = ""
local function build()
    if mainfile == "" then
        mainfile = vim.fn.input({prompt = "Main File: ", completion = "file"})
    end
    return mainfile
end

vim.keymap.set('n', '<Leader>bp', build, {})
vim.keymap.set('n', '<Leader>bu', function () mainfile = "" end, {})

------------------------------------------------------------------------------
-- Font changes
------------------------------------------------------------------------------
local fontSize = 11
local function changeFontSize(val)
    fontSize = fontSize + val
    vim.opt.guifont = "Consolas:h" .. tostring(fontSize)
    print("Setting font to " .. fontSize)
end
local darkBackground = true
local function toggleBackground()
    darkBackground = not darkBackground
    vim.opt.background = (darkBackground and "dark" or "light")
end

vim.keymap.set('n', '<C-=>', function () changeFontSize(1) end, {})
vim.keymap.set('n', '<C-/>', function () changeFontSize(-1) end, {})
vim.keymap.set('n', '<C-\\>', toggleBackground, {})
