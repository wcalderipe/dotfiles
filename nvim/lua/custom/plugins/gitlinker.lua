-- Git shareble permalinks of files.
return {
  'ruifm/gitlinker.nvim',
  config = function()
    require('gitlinker').setup()

    local function set_yank_keymap_for_mode(mode)
      vim.keymap.set(mode, '<leader>gy', function()
        require('gitlinker').get_buf_range_url(mode)
      end, { desc = '[G]it [y]ank permalink' })
    end

    -- Copy the permalink of the current line.
    set_yank_keymap_for_mode 'n'
    -- Copy the permalink of the current line range.
    set_yank_keymap_for_mode 'v'
  end,
}
