local Common = {}

Common.isWindows = string.find(vim.loop.os_uname().sysname:lower(), "windows")

return Common
