# -----------------------------------------------------------------------------
# Oh my ZSH
# -----------------------------------------------------------------------------

# Path to your oh-my-zsh installation.
export ZSH=/Users/entrox/.oh-my-zsh

COMPLETION_WAITING_DOTS="true"

plugins=(git shrink-path)

source $ZSH/oh-my-zsh.sh


# -----------------------------------------------------------------------------
# Aliases
# -----------------------------------------------------------------------------

alias ls='ls -hG'
alias l='ls -lhG'
alias rm='rm -i'
alias cdd=cd
alias mkdir='nocorrect mkdir'
alias dirsize="du -skh *(/)"


# -----------------------------------------------------------------------------
# General Environment
# -----------------------------------------------------------------------------

export LANG=en_US.UTF-8

export PATH=/usr/local/bin:/opt/homebrew/bin:$HOME/.local/bin:/Applications/Postgres.app/Contents/Versions/latest/bin:$PATH

# match colors between ls and tab completion
export LSCOLORS='exgxfxdxcxegedabagexex'
zstyle ':completion:*' list-colors 'di=34:ln=36:so=35:pi=33:ex=32:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=34:ow=34'

setopt prompt_subst
PS1='[%{${fg[green]}%}%n@%m%{${fg[default]}%}]:$(shrink_path -f)$ '

# enable direnv if available
if command -v direnv &> /dev/null; then
    eval "$(direnv hook zsh)"
    # Remove _direnv_hook from chpwd_functions to prevent double execution
    chpwd_functions=(${chpwd_functions[@]:#_direnv_hook})
fi


# -----------------------------------------------------------------------------
# Python environment integration
# -----------------------------------------------------------------------------

export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - zsh)"


# -----------------------------------------------------------------------------
# Editor and Emacs integration
# -----------------------------------------------------------------------------

export EDITOR=emacsclient
alias e='$EDITOR'

vterm_printf() {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}

setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

find_file() {
    vterm_cmd find-file "$(realpath "${@:-.}")"
}


# -----------------------------------------------------------------------------
# Initialize completion
# -----------------------------------------------------------------------------

fpath+=~/.zfunc; autoload -Uz compinit; compinit
