let g:ale_sign_error = 'E'
let g:ale_sign_warning = 'W'

let g:ale_linters = {
\ 'javascript': ['eslint'],
\ 'typescript': ['tsserver'],
\ }

let g:ale_fixers = {
\ 'javascript': ['prettier'],
\ 'typescript': ['prettier'],
\ }

