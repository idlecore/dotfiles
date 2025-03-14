local builtin = require('telescope.builtin')
local idlecore_common = require("idlecore.common")

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

if  idlecore_common.isWindows then

    -- Open vim config
    noremap('n', '<Leader>vc', ':tabe ~/AppData/Local/nvim<CR>')

    -- Open terminal at the position of the current file *WINDOWS*
    noremap('n', '<Leader>t', ':!wt pwsh -WorkingDirectory %:p:h<CR>')

else

    -- Open vim config
    noremap('n', '<Leader>vc', ':tabe ~/.config/nvim<CR>')

end


------------------------------------------------------------------------------
-- Telescope
------------------------------------------------------------------------------
vim.keymap.set('n', '<Leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<Leader>fp', builtin.git_files, {})
vim.keymap.set('n', '<Leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<Leader>fr', function () builtin.lsp_references({fname_width=100}) end, {})

------------------------------------------------------------------------------
-- Build scripts
------------------------------------------------------------------------------
local mainfile = ""
local function build_python()
    if mainfile == "" then
        mainfile = vim.fn.input({prompt = "Main File: ", completion = "file"})
    else
        vim.cmd("!python " .. mainfile)
    end
    return mainfile
end

vim.keymap.set('n', '<Leader>bp', build_python, {})
vim.keymap.set('n', '<Leader>bu', function () mainfile = "" end, {})
vim.keymap.set('n', '<Leader>br', ":!cargo build<CR>")
vim.keymap.set('n', '<Leader>bvs', ":!msbuild<CR>")

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

------------------------------------------------------------------------------
-- Todo Lists
------------------------------------------------------------------------------
-- Creates and manages todolist in the following form:
-- "<WORD> [ ] <Some Item>" for example: "// [ ] - This is a todolist item"

-- Check item
noremap('n', '<Leader>tc', 'mt0wlrx`t')
-- Uncheck item
noremap('n', '<Leader>tu', 'mt0wlr `t')


------------------------------------------------------------------------------
-- Autocomplete
------------------------------------------------------------------------------
-- noremap('i', '<C-Enter>', )

-- Copilot
vim.keymap.set('i', '<C-Enter>', 'copilot#Accept("\\<CR>")', {
    expr = true,
    replace_keycodes = false
})
vim.g.copilot_no_tab_map = true

vim.keymap.set('i', '<C-l>', '<Plug>(copilot-next)')
vim.keymap.set('i', '<C-h>', '<Plug>(copilot-previous)')
vim.keymap.set('i', '<C-s>', '<Plug>(copilot-suggest)')

vim.g.copilot_enabled = false

noremap('n', '<Leader>cc', ':CopilotChat<CR>')
noremap('n', '<Leader>cp', ':CopilotChatPrompt<CR>')
