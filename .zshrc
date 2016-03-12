# Path to your oh-my-zsh installation.
  export ZSH=~/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git svn mercurial
    # j command (apt-get install autojump)
    autojump
    # If a command is not recognized in the $PATH, this will use Ubuntu's
    # command-not-found package to find it or suggest spelling mistakes:
    command-not-found
    # navigate the history of previous current-working-directories using
    # ALT-LEFT and ALT-RIGHT
    dirhistory
)

# User configuration

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
alias xi="xclip -i -selection clipboard"
alias xo="xclip -o -selection clipboard"

[ -f ~/.bash_aliases ] && source $HOME/.bash_aliases
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

#
# User defined tweaks
#

# ALT-X like in Emacs to list available commands
fzf-locate-widget() {
    local selected
    if selected=$(for i in ${(k)commands} ${(k)functions} ${(k)aliases}; do echo $i; done | fzf -q "$LBUFFER"); then
        LBUFFER=$selected
    fi
    zle redisplay
}
zle     -N    fzf-locate-widget
bindkey '\ex' fzf-locate-widget

# Integrate kill-buffer with X clipboard
# https://gist.github.com/welldan97/5127861
pb-kill-line () {
  zle kill-line
  echo -n $CUTBUFFER | xi
}

pb-kill-whole-line () {
  zle kill-whole-line
  echo -n $CUTBUFFER | xi
}

pb-backward-kill-word () {
  zle backward-kill-word
  echo -n $CUTBUFFER | xi
}

pb-kill-word () {
  zle kill-word
  echo -n $CUTBUFFER | xi
}

pb-kill-buffer () {
  zle kill-buffer
  echo -n $CUTBUFFER | xi
}

pb-copy-region-as-kill-deactivate-mark () {
  zle copy-region-as-kill
  zle set-mark-command -n -1
  echo -n $CUTBUFFER | xi
}

pb-yank () {
  CUTBUFFER=$(xo)
  zle yank
}

zle -N pb-kill-line
zle -N pb-kill-whole-line
zle -N pb-backward-kill-word
zle -N pb-kill-word
zle -N pb-kill-buffer
zle -N pb-copy-region-as-kill-deactivate-mark
zle -N pb-yank

bindkey '^K'   pb-kill-line
bindkey '^U'   pb-kill-whole-line
bindkey '\e^?' pb-backward-kill-word
bindkey '\e^H' pb-backward-kill-word
bindkey '^W'   pb-backward-kill-word
bindkey '\ed'  pb-kill-word
bindkey '\eD'  pb-kill-word
bindkey '^X^K' pb-kill-buffer
bindkey '\ew'  pb-copy-region-as-kill-deactivate-mark
bindkey '\eW'  pb-copy-region-as-kill-deactivate-mark
bindkey '^Y'   pb-yank

# Use ranger to change current directory. Prevent recursive ranger.
#
# https://raw.githubusercontent.com/bitterjug/dotfiles/master/bash/rangercd.sh
# https://wiki.archlinux.org/index.php/ranger
ranger-cd() {
    if [ -z "$RANGER_LEVEL" ]
    then
        tempfile=$(mktemp)
        ranger --choosedir="$tempfile" "${@:-$(pwd)}" < $TTY
        test -f "$tempfile" &&
            if [ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]; then
                cd -- "$(cat "$tempfile")"
            fi
        rm -f -- "$tempfile"
    else
        exit
    fi
}

# Bind Ctrl-O to ranger-cd. If ranger uses same key for entering shell then we
# will obtain consistent ranger-console switching.
zle -N ranger-cd
bindkey '^o' ranger-cd
alias rg=ranger-cd
