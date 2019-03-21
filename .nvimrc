let g:ale_linters = {
\   'sh': ['shellcheck'],
\   'haskell': ['hlint'],
\}

let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'haskell': 'stylish-haskell',
\}
