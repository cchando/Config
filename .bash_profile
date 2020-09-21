# # ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc
if [ -e /home/cameron/.nix-profile/etc/profile.d/nix.sh ]; then . /home/cameron/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

put the current git branch in the command prompt
parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* (.*)/(1)/'
}

export PS1="[33[00m]u@h[33[01;34m] W [33[31m]$(parse_git_branch) [33[00m]$[33[00m] "

export PATH="$PATH:$HOME:$HOME/.local/bin:$HOME/.config/yarn/global/node_modules/.bin:$HOME/programs/search-nixpkgs"
export AVOUTPUT="$HOME/AVOutput"
export XDG_CONFIG_HOME="$HOME/.config"
export TERM="kitty" #termite
export BROWSER="google-chrome-stable"
export EDITOR="vim"
export GIT_EDITOR="vim"
export VISUAL="vim"
export NIXPKGS_ALLOW_UNFREE=1
