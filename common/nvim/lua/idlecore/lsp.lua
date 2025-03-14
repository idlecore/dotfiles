
local lsp_zero = require('lsp-zero')

lsp_zero.on_attach(function(client, bufnr)
    lsp_zero.default_keymaps({buffer = bufnr})
end)

require('mason').setup({})
require('mason-lspconfig').setup({
    ensure_installed = {
        -- Common
        "ast_grep",         -- Code structural search
        "harper_ls",        -- Grammar/Spellcheck
        -- Languages
        "clangd",           -- C/C++
        "lua_ls",           -- Lua
        "clojure_lsp",      -- Clojure
        -- "zprint-clj",       -- Clojure formatting
        "cmake",            -- Cmake
        "gopls",            -- Golang
        "pylsp",            -- Python
        "rust_analyzer",    -- Rust
        "svelte",           -- Svelte and Sveltekit
        "tailwindcss"       -- Tailwind
    },
    handlers = {
        lsp_zero.default_setup,
    },
})
