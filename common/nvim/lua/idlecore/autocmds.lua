local autocmd_group = vim.api.nvim_create_augroup("Custom auto-commands", {clear = true})
vim.api.nvim_create_autocmd({ "BufWritePost" },
{
    pattern = {"*.clj", "*.cljs"},
    desc = "Auto-format Clojure files.",
    callback = function()
        local filename = vim.api.nvim_buf_get_name(0)
        vim.cmd(":silent !zprint-clj.cmd -i " .. filename .. " -o ./")
    end,
    group = autocmd_group
})
