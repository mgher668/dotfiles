alias lg="lazygit"
alias v="nvim"
alias l="exa"
alias ls="exa"
alias ll="exa -l"
alias lla="exa -l -a"
alias enw="emacs -nw"
alias s="kitty +kitten ssh"
alias sshq='ssh $(awk "/^Host / {print \$2}" ~/.ssh/config | grep -v "*" | sort -u | fzf --height 40% --reverse --border)'
alias lv="NVIM_APPNAME=lazyvim nvim"

if status is-interactive
    if command -q thefuck
        thefuck --alias | source
    end

    if command -q zoxide
        zoxide init fish | source
    end

    bind -M insert \cf accept-autosuggestion
end

if status is-interactive
    # Commands to run in interactive sessions can go here
end

# .env
if test -f ~/.env
    source ~/.env
end

# direnv (~/.envrc)
if type direnv >/dev/null
    eval (direnv hook fish)
end

function fish_mode_prompt
    echo ''
end

# function nvm
#   bass source ~/.nvm/nvm.sh --no-use ';' nvm $argv
# end

# load_nvm

set -gx EDITOR nvim
set -gx VISUAL nvim

set --export PATH $HOME/.cargo/bin $PATH

set --export PATH $HOME/.local/bin $PATH

# set --export PATH $HOME/bin $PATH

set --export PATH $HOME/.yarn/bin $PATH

# keymap
# bind \cs accept-autosuggestion
bind -M insert \cf accept-autosuggestion

# bun
set --export BUN_INSTALL "$HOME/.bun"
set --export PATH $BUN_INSTALL/bin $PATH
# set --export JAVA_HOME "/usr/lib/jvm/java-8-openjdk/jre"
# set -gx XDG_CURRENT_DESKTOP Unity
# set -gx DESKTOP_SESSION Unity

# pnpm
set -gx PNPM_HOME "/home/mgher/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
    set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end
starship init fish | source

# opencode
fish_add_path /home/mgher/.opencode/bin

# Added by jcode installer
if not contains "/home/mgher/.local/bin" $PATH
    set -gx PATH "/home/mgher/.local/bin" $PATH
end
