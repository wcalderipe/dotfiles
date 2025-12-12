return {
  'ThePrimeagen/harpoon',
  branch = 'harpoon2',
  dependencies = {
    'nvim-lua/plenary.nvim',
  },
  config = function()
    local harpoon = require 'harpoon'

    harpoon:setup()

    vim.keymap.set('n', '<leader>h', function()
      harpoon:list():add()
    end, {
      desc = 'Add file to harpoon',
    })

    vim.keymap.set('n', '<C-e>', function()
      harpoon.ui:toggle_quick_menu(harpoon:list())
    end, {
      desc = 'Toggle harpoon quick menu',
    })

    for i = 1, 5 do
      vim.keymap.set('n', '<leader>' .. i, function()
        harpoon:list():select(i)
      end, {
        desc = 'Go to harpoon file ' .. i,
      })
    end
  end,
}
