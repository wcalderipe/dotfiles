-- Git management in a buffer.
-- Inspired by the famous Magit from Emacs.
return {
  'NeogitOrg/neogit',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'sindrets/diffview.nvim',
    'nvim-telescope/telescope.nvim',
  },
  config = function()
    local neogit = require 'neogit'

    neogit.setup {
      kind = 'tab',
    }

    vim.keymap.set('n', '<leader>gs', function()
      neogit.open()
    end, { desc = '[G]it [s]tatus' })
  end,
}
