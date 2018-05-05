" runs neomake when save a file
autocmd! BufWritePost * Neomake

let g:neomake_javascript_enabled_makers = ['eslint', 'flow']
let g:neomake_jsx_enabled_makers = ['eslint', 'flow']
let g:neomake_open_list = 0
let g:neomake_error_sign = {'text': 'E', 'texthl': 'ErrorMsg'}
let g:neomake_warning_sign = {'text': 'W', 'texthl': 'WarningMsg'}
let g:neomake_message_sign = {'text': 'M', 'texthl': 'MessageMsg'}
let g:neomake_info_sign = {'text': 'I', 'texthl': 'InfoMsg'}

function StrTrim(txt)
  return substitute(a:txt, '^\n*\s*\(.\{-}\)\n*\s*$', '\1', '')
endfunction

" get local flow or global
let g:flow_path = StrTrim(system('PATH=$(npm bin):$PATH && which flow'))
if g:flow_path != ''
  let g:neomake_javascript_flow_exe = g:flow_path
endif

" get local eslint or global 
let g:eslint_path = StrTrim(system('PATH=$(npm bin):$PATH && which eslint'))
if g:eslint_path != ''
  let g:neomake_javascript_eslint_exe = g:eslint_path
endif
