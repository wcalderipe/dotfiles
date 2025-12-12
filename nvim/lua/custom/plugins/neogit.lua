-- Git management in a buffer.
-- Inspired by the famous Magit from Emacs.
return {
  'NeogitOrg/neogit',
  dependencies = {
    'nvim-lua/plenary.nvim',
    'nvim-telescope/telescope.nvim',
  },
  config = function()
    local neogit = require 'neogit'

    neogit.setup {
      -- If neogit is on `tab` mode, when searching and navigating to a buffer
      -- using telescope it'll open in the current tab which is the expected
      -- behavior but an annoying one because it never closes them. Using
      -- `replace` stacks the buffer on top and buffer navigation will just
      -- work.
      kind = 'vsplit',
      mappings = {
        status = {
          -- Disable closing on "q" in the status buffer because it often
          -- triggers the record macro and it's annoying. I want ONE way to
          -- close buffers.
          ['q'] = false,
        },
      },
    }

    vim.keymap.set('n', '<leader>gs', function()
      neogit.open()
    end, { desc = '[G]it [s]tatus' })
  end,
}
