-- File system management in a buffer.
--
return {
  'stevearc/oil.nvim',
  config = function()
    require('oil').setup {
      default_file_explorer = true,
      skip_confirm_for_simple_edits = true,
      view_options = {
        show_hidden = true,
      },
    }
  end,

  vim.keymap.set('n', '-', '<CMD>Oil<CR>', { desc = 'Open parent directory' }),
}
