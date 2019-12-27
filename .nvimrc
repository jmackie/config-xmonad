let g:ale_linters = {
\   'sh': ['shellcheck'],
\   'haskell': ['ghcide', 'hlint'],
\}

let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'sh': 'shfmt',
\   'nix': 'Nixfmt',
\   'haskell': 'Ormolu',
\   'yaml': 'prettier',
\}

call ale#linter#Define('haskell', {
\   'name': 'ghcide',
\   'lsp': 'stdio',
\   'executable': 'ghcide',
\   'command': 'ghcide --lsp',
\   'project_root': '.',
\})

function! Ormolu(buffer) abort
    return { 'command': 'ormolu' }
endfunction

function! Nixfmt(buffer) abort
    return { 'command': 'nixfmt' }
endfunction
