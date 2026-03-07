# Fish Shell Compatibility

The user's shell is Fish. All shell commands and snippets must be Fish-compatible. Avoid Bash/Zsh-isms that don't work in Fish:

- No here-documents (`<<EOF ... EOF`). Use `echo` or `printf` piped to the command, or write to a file with file-writing tools.
- No `export VAR=value`. Use `set -x VAR value`.
- No `$(...)` for command substitution. Use `(...)`.
- `&&` and `||` are supported in Fish 3.0+. Use them freely.
- No Bash-style `function name() { ... }`. Use `function name ... end`.
- No `source ~/.bashrc` or `.bash_profile`. Fish config lives at `~/.config/fish/config.fish`.
- No `${}` variable expansion syntax. Fish uses plain `$VAR` or `{$VAR}` for disambiguation.
- No arrays with `=(...)`. Fish uses `set mylist a b c`.
