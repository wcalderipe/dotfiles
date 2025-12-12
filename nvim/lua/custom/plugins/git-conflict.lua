-- Visualise and resolve conflicts.
return {
  'akinsho/git-conflict.nvim',
  version = '*',
  config = function()
    require('git-conflict').setup {
      default_mappings = true,
      default_commands = true,
      disable_diagnostics = false,
      list_opener = 'copen', -- command or function to open the conflicts list
      highlights = { -- They must have background color, otherwise the default color will be used
        incoming = 'DiffAdd',
        current = 'DiffText',
      },
    }
  end,
}
