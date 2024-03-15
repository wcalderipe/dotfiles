return {
  'kevinhwang91/nvim-ufo',
  dependencies = { 'kevinhwang91/promise-async' },
  config = function()
    -- Folding configuration.
    vim.opt.fillchars = { fold = ' ' }
    vim.opt.foldmethod = 'indent'
    vim.opt.foldlevel = 99
    vim.opt.foldcolumn = '0'

    vim.keymap.set('n', 'zR', require('ufo').openAllFolds, { desc = 'Open all folds' })
    vim.keymap.set('n', 'zM', require('ufo').closeAllFolds, { desc = 'Close all folds' })
    vim.keymap.set('n', 'zK', function()
      local winid = require('ufo').peekFoldedLinesUnderCursor()

      if not winid then
        vim.lsp.buf.hover()
      end
    end, { desc = 'Peek fold' })

    require('ufo').setup {
      provider_selector = function()
        return { 'lsp', 'indent' }
      end,
    }
  end,
}
