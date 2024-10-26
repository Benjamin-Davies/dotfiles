require('packer').startup(function(use)
	use 'wbthomason/packer.nvim'

	-- Basic editing
	use 'tpope/vim-repeat'
	use 'tpope/vim-surround'

	-- Languages
	use 'preservim/vim-markdown'

	-- Colors
	use 'Mofiqul/adwaita.nvim'
	use {
		'nvim-treesitter/nvim-treesitter',
		run = ':TSUpdate',
	}
	
	-- Navigation
	use {
		'nvim-telescope/telescope.nvim',
		branch = '0.1.x',
		requires = { 'nvim-lua/plenary.nvim' },
	}

	-- Integration
	use 'tpope/vim-fugitive'
	use 'Josiah-tan/plover-vim-tutor'
end)
