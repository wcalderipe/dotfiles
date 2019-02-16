" Disable error and warning background highlight
let g:ale_set_highlights = 0

let g:ale_sign_error = 'E'
let g:ale_sign_warning = 'W'

let b:ale_linters = {
\ 'javascript': ['eslint'],
\ 'typescript': ['tsserver', 'tslint', 'prettier'],
\ }

let b:ale_fixers = {
\ 'javascript': ['prettier'],
\ 'typescript': ['prettier'],
\ }
