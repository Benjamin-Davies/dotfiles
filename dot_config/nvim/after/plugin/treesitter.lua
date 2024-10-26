require('nvim-treesitter.configs').setup {
	ensure_installed = { 'javascript', 'typescript', 'c', 'lua', 'rust' },

	sync_install = false,
	auto_install = true,
}
