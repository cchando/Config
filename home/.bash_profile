# # ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

#put the current git branch in the command prompt
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* (.*)/(1)/'
}


export PS1="[33[00m]u@h[33[01;34m] W [33[31m]$(parse_git_branch) [33[00m]$[33[00m] "

export WLR_DRM_NO_MODIFIERS=1 # fixes monitor issue w/ sway
export PATH="$PATH:$HOME:$HOME/.local/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/programs/"
export AVOUTPUT="$HOME/AVOutput"
export XDG_CONFIG_HOME="$HOME/.config"
export TERM="kitty"
export BROWSER="google-chrome-stable"
export EDITOR="vim"
export GIT_EDITOR="vim"
export VISUAL="vim"
export DOCUMENTS="$HOME/Documents"
export MUSIC="$HOME/Music"
export PICTURES="$HOME/Pictures"
export PROGRAMS="$HOME/Programs"
export VIDEOS="$HOME/Videos"

# source /home/cam/.config/broot/launcher/bash/br

