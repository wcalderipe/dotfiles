return {
  'mistweaverco/kulala.nvim',
  dependencies = { 'which-key.nvim' },
  ft = 'http',
  opts = {},
  init = function()
    vim.filetype.add {
      extension = {
        http = 'http',
      },
    }
  end,
  config = function(_, opts)
    require('kulala').setup(opts)

    -- Register which-key mappings for HTTP files
    vim.api.nvim_create_autocmd('FileType', {
      pattern = 'http',
      callback = function(event)
        require('which-key').register({
          ['<leader>r'] = {
            name = '󰖟 HTTP Request',
            r = {
              function()
                require('kulala').run()
              end,
              '󱓞 Execute request',
            },
            ['['] = {
              function()
                require('kulala').jump_prev()
              end,
              '󰒮 Previous request',
            },
            [']'] = {
              function()
                require('kulala').jump_next()
              end,
              '󰒭 Next request',
            },
            i = {
              function()
                require('kulala').inspect()
              end,
              '󰋼 Inspect request',
            },
            c = {
              function()
                require('kulala').from_curl()
              end,
              '󰆏 Convert curl command',
            },
          },
        }, { buffer = event.buf })
      end,
    })
  end,
}
