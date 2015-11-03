if [[ $(uname) == 'Darwin' ]]; then
    EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
    EMACSCLIENT=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient
    alias emacs="$EMACS"
    alias emacsclient="$EMACSCLIENT"
else
    EMACS=emacs
    EMACSCLIENT=emacsclient
fi

emacs-venv-activate(){
    local venv=~/.emacs.d/emacs-env
    if [[ -f "$venv/bin/activate" && -z "$VIRTUAL_ENV" ]]; then
	echo "using virtualenv in $venv"
	source "$venv/bin/activate"
    else
	echo "no virtualenv"
    fi
}

edaemon(){
    rm -f ~/.emacs.desktop.lock ~/.emacs.d/.emacs.desktop.lock
    (cd ~ && "$EMACS" --daemon)
}

ec(){
    "$EMACSCLIENT" -c "$@" &
}

enw(){
    "$EMACSCLIENT" -nw "$@"
}

e(){
    # open file in an existing server process
    re='^[0-9]+$'
    if [[ $2 =~ $re ]]; then
	"$EMACSCLIENT" -n +$2:0 $1
    else
	"$EMACSCLIENT" -n "$@"
    fi
}

# if [[ $(basename $SHELL) == "zsh" ]]; then
#     compdef ec=ls
#     compdef enw=ls
#     compdef e=ls
# fi
