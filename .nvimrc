let g:ale_linters = {
\   'sh': ['shellcheck'],
\   'haskell': ['ghcide', 'hlint'],
\}

let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'sh': 'shfmt',
\   'nix': [{buffer -> { 'command': 'nixfmt' }}],
\   'haskell': [{buffer -> { 'command': 'ormolu' }}],
\   'yaml': 'prettier',
\}
