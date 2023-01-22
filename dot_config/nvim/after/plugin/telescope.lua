local builtin = require('telescope.builtin')
vim.keymap.set('n', '<leader> ', builtin.git_files)
vim.keymap.set('n', '<leader>,', builtin.buffers)
vim.keymap.set('n', '<leader>ff', builtin.find_files)
