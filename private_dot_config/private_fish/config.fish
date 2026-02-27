alias lg="lazygit"
alias v="nvim"
alias l="exa"
alias ls="exa"
alias ll="exa -l"
alias lla="exa -l -a"
alias enw="emacs -nw"
alias s="kitty +kitten ssh"

thefuck --alias | source

if status is-interactive
    # Commands to run in interactive sessions can go here
end

# .env
if test -f ~/.env
  source ~/.env
end

# direnv (~/.envrc)
if type direnv > /dev/null
    eval (direnv hook fish)
end

function fish_mode_prompt
  echo ''
end

function nvm
  bass source ~/.nvm/nvm.sh --no-use ';' nvm $argv
end

# load_nvm

set -gx EDITOR nvim
set -gx VISUAL nvim

set --export PATH $HOME/.cargo/bin $PATH

set --export PATH $HOME/.local/bin $PATH

# set --export PATH $HOME/bin $PATH

set --export PATH $HOME/.yarn/bin $PATH

zoxide init fish | source

# keymap
# bind \cs accept-autosuggestion
bind -M insert \cf accept-autosuggestion

# bun
set --export BUN_INSTALL "$HOME/.bun"
set --export PATH $BUN_INSTALL/bin $PATH
# set --export JAVA_HOME "/usr/lib/jvm/java-8-openjdk/jre"
