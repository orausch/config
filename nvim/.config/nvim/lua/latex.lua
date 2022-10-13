vim.api.nvim_create_autocmd("FileType", {
  pattern = "tex",
  callback = function()
    -- error formats for textidote
    vim.opt_local.errorformat = ""
    vim.opt_local.errorformat:append([[%f(L%lC%c-L%\\d%\\+C%\\d%\\+):\ %m]])
    vim.opt_local.errorformat:append([[%f(L%lC%c-\?):\ %m]])
    vim.opt_local.errorformat:append([[%-G%.%#]])
  end
})
