return {
  {
    'FabijanZulj/blame.nvim',
    lazy = false,
    config = function()
      require('blame').setup {}
    end,
    keys = {
      {
        '<leader>gb',
        '<cmd>BlameToggle<CR>',
        desc = 'Toggle [g]it [b]lame',
      },
    },
  },
}
