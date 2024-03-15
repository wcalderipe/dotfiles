-- Git management in a buffer.
-- Inspired by the famous Magit from Emacs.
return {
  'NeogitOrg/neogit',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'sindrets/diffview.nvim',
    'nvim-telescope/telescope.nvim',
  },
  -- TODO: Using lazy load for some reason doesn't register the
  -- keymap.
  -- lazy = true,
  config = function()
    local neogit = require 'neogit'

    neogit.setup {}

    vim.keymap.set('n', '<leader>Gs', function()
      neogit.open()
    end, { desc = 'Git status' })
  end,
}
