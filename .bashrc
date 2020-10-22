#
# ~/.bashrc
#
. $HOME/.bash_aliases 		#source bash_aliases
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# If running in tty1, run startx
if [ "$(tty)" = "/dev/tty1" ]; then startx; fi

shopt -s extglob dotglob globstar

# # Notes
#stat -- display file permissions -- stat -c %A %n
#lspci -- list PCI bus devices
#ping 8.8.8.8
#sftp cameron@10.0.0.2
#xdg-mime  #check / modify file-type associations
#wpa_passphrase Frontier4704 21422325889218 > /etc/wpa_supplicant.conf #5148275597
#alias ni='nix-instantiate --eval'
#ip a -- get status of wireless devices
#xinput list

# Nix & NixOS
alias cr='less ~/.nixenv | grep'   # check nixpkgs repo file
alias crd='nix-env -qaP --description | grep'   # check nixpkgs repo
alias rb='sudo nixos-rebuild switch'  # rebuild NixOS
alias clean='nix-collect-garbage -d'
alias upd='nix-channel --update'
alias upg='nix-env --upgrade'
alias upgv='nix-env --upgrade --always'
alias rollback='nix-channel --rollback'
alias rm='mv -t /home/cameron/.trash'
alias rmu='mv -t /home/cameron/.trash' # remove unsafe
alias inst='nix-env -iA'
alias lgen='nix-env --list-generations'
alias sgen='nix-env --switch-generation'
alias unin='nix-env --uninstall'
alias linst='nix-env -q --installed'
alias lhave='nix-env -q'
# alias anp='nix-env -f '<nixpkgs>' -iA'   # nodePackages.searchterm

# program aliases
alias e='exit'
alias c='clear'
alias ns='nix-shell'
alias charmap='gucharmap'
alias grep='egrep'
alias mv='mv -i'
alias cp='cp -i'
alias sudo='sudo '
alias ls='ls --color=auto'
alias kill='kill -9'
alias xflux='xflux -z 75044'
alias redshift='redshift -l 32.96:-96.67 -t 6500:2000'

# wifi
alias conh='sudo nmcli device wifi connect Frontier4704 password 21422325889218' #5148275597
alias con='nmcli device wifi connect'
alias wifi='nmcli device wifi' # list wifi networks
alias wpabg='sudo wpa_supplicant -i wlp1s0 -c f -B'  # (-d for debugging, -B for background/daemon)
alias ipsu='sudo ip link set wlp1s0 up'
alias aip='sudo ip addr add 10.0.0.1/8 dev enp0s31f6'
# alias wpastart='sudo systemctl start wpa_supplicant' # I think this doesn't work -- wpa is controlled by systemd

# convenience
alias et='emacs -nw'
alias vimode='set -o vi'
alias tr='racket -I typed/racket'
alias ra='racket'
alias fonts='gucharmap'
alias lfonts='gtk2fontsel'
alias lst='ps -A | grep'
alias xr='xrdb -merge ~/.Xdefaults'
alias bat='acpi'    # or 'upower ...'
alias music='cmuse'
alias zip='7z a'
alias unzip='7z x'
alias zipt='7z a -tzip'
alias bhi='sudo brightnessctl set 100%'
alias bmhi='sudo brightnessctl set 75%'
alias bmed='sudo brightnessctl set 50%'
alias bmlo='sudo brightnessctl set 25%'
alias blo='sudo brightnessctl set 10%'
alias pade="xinput set-prop "$(xinput list | grep -i touchpad | cut -f 2 | grep -oE '[[:digit:]]+')" 'Device Enabled' 1"
alias sticke="xinput set-prop "$(xinput list | grep -i stick | cut -f 2 | grep -oE '[[:digit:]]+')" 'Device Enabled' 1"
function conda-shell {
    nix-shell ~/.conda-shell.nix
}
#alias mvfiles='find . type f -regextype egrep -regex '.*[a-z]+\.el$' -execdir mv -t dest {} \+'  # example only

# config edits
alias eba='emacs ~/.bashrc'
alias evi='emacs ~/.vimrc'
alias eiwm='emacs ~/.i3/config'
alias eis='emacs ~/.i3status.conf'
alias ete='emacs ~/.config/termite/config'
alias eki='emacs ~/.config/kitty/kitty.conf'
alias etm='emacs ~/.tmux.conf'
alias est='emacs ~/.config/stretchly/config.json'
alias enc='sudo vim /etc/nixos/configuration.nix'
# alias es='vim ~/.config/sway/config'

# git commands
alias com="git commit"
alias st="git status"
alias ch="git checkout"
alias sw="git switch"
alias br="git branch"
alias pull="git pull"
alias pullr="git pull --rebase"
alias push="git push"
alias add="git add"
alias amend="git commit amend"
alias reba="git rebase"
alias rebase="git rebase"
alias log="git log"
alias logp="git log --pretty=oneline"
alias logpp="git log --all --decorate --oneline --graph"
alias ltracked='git ls-tree -r --name-only' #list tracked files in given branch
alias tag="git tag"



PS1='[\u@\h \W]\$ '

stty -ixon

