# Check for an interactive session
[ -z "$PS1" ] && return

PS1="\[\033[0;34m\]\W\[\033[0m\]\[\033[1;32m\]>\[\033[0m\] "

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias yum='yum --color=auto'
alias ff='nohup firefox --no-remote -P $1 >/dev/null 2>&1 &'
alias hgrep='history | grep'
complete -cf sudo

# Don't save repeated commands in bash history
export HISTCONTROL="ignoredups"

# Disable ^S/^Q flow control (does anyone like/use this at all?)
stty -ixon

# autocorrects cd misspellings, eg. from 'cd /sur/src/linus', shell
# would correctly guess 'cd /usr/src/linux'
shopt -s cdspell

# checks window size after command execution -> prevents wrapping
# problems after window resize
shopt -s checkwinsize

# arch linux package manager search
alias pacs="pacsearch"
pacsearch ()
{
       echo -e "$(pacman -Ss $@ | sed \
       -e 's#core/.*#\\033[1;31m&\\033[0;37m#g' \
       -e 's#extra/.*#\\033[0;32m&\\033[0;37m#g' \
       -e 's#community/.*#\\033[1;35m&\\033[0;37m#g' \
       -e 's#^.*/.* [0-9].*#\\033[0;36m&\\033[0;37m#g' )"
}

# apply a simple command to multiple args
apply ()
{
    cmd=$1
    shift
    for x in $*; do $cmd $x; done
}

# extract various archives w/o fiddling around
ext ()
{
    if [ -f $1 ] ; then
        case $1 in
            *.7z)        7z x $1       ;;
            *.bz2)       bunzip2 $1    ;;
            *.gz)        gunzip $1     ;;
            *.rar)       rar x $1      ;;
            *.tar.bz2)   tar xjf $1    ;;
            *.tar.gz)    tar xzf $1    ;;
            *.tar)       tar xf $1     ;;
            *.tbz2)      tar xjf $1    ;;
            *.tgz)       tar xzf $1    ;;
	    *.xz)        xz -d $1      ;;
            *.zip)       unzip $1      ;;
            *.Z)         uncompress $1 ;;
            *)           echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# mkmv - creates a new directory and moves the file into it, in 1 step
# Usage: mkmv <file> <directory>
mkmv()
{
    mkdir "$2"
    mv "$1" "$2"
}

# sanitize - set file/directory owner and permissions to normal values (644/755)
# Usage: sanitize <file>
sanitize()
{
    chmod -R u=rwX,go=rX "$@"
    chown -R ${USER}.users "$@"
}

# nh - run command detached from terminal and without output
# Usage: nh <command>
nh()
{
    nohup "$@" &>/dev/null &
}
