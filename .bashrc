# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Source global definitions
if [ -f /etc/bashrc ]; then
	source /etc/bashrc
fi

complete -cf sudo
complete -cf man

# Don't save repeated commands in bash history
export HISTCONTROL="ignoredups"

# autocorrects cd misspellings, eg. from 'cd /sur/src/linus', shell
# would correctly guess 'cd /usr/src/linux'
shopt -s cdspell

alias openports="netstat --all --numeric --programs --inet"
alias ls='ls --color=auto'
# safety features
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -I' # 'rm -i' prompts for every file
alias ln='ln -i'
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

# kerl completion stuff
source "$HOME/.bash_completion"

# similar to opam switch
source "$HOME/bin/erlang/22.0/activate"

test -s "$HOME/.kiex/scripts/kiex" && source "$HOME/.kiex/scripts/kiex"

# same for Elixir
source "$HOME/.kiex/elixirs/elixir-1.9.1.env"

# use fzf
source /usr/share/fzf/completion.bash

# apply a simple command to multiple args
function apply() {
    cmd=$1
    shift
    for x in $*; do $cmd $x; done
}

# mkgo - create a new directory and cd into it
# NOTE: calling this function "mkcd" causes the function to fail, as mkdir seems to be passed an empty string. 
function mkgo() {
    mkdir "$1"
    cd "$1"
}

# mkmv - creates a new directory and moves the file into it, in 1 step
# Usage: mkmv <file> <directory>
function mkmv() {
    mkdir "$2"
    mv "$1" "$2"
}

# sanitize - set file/directory owner and permissions to normal values (644/755)
# Usage: sanitize <file>
function sanitize() {
    chmod -R u=rwX,go=rX "$@"
    chown -R ${USER}.users "$@"
}

# nh - run command detached from terminal and without output
# Usage: nh <command>
function nh() {
    nohup "$@" &>/dev/null &
}
