#
# ~/.bashrc
#
. $HOME/.bash_aliases 		#source bash_aliases
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# If running in tty1, run startx
if [ "$(tty)" = "/dev/tty1" ]; then startx; fi

function conda-shell {
    nix-shell ~/.conda-shell.nix
}
#stat -- display file permissions -- stat -c %A %n
#lspci -- list PCI bus devices
#ping 8.8.8.8
#sftp cameron@10.0.0.2
alias vimmode='set -o vi'
#alias ni='nix-instantiate --eval'
alias tracket='racket -I typed/racket'
alias aip='sudo ip addr add 10.0.0.1/8 dev enp0s31f6'
#xinput list
shopt -s extglob dotglob globstar
alias padd="xinput set-prop "$(xinput list | grep -i touchpad | cut -f 2 | grep -oE '[[:digit:]]+')" 'Device Enabled' 0"
alias pade="xinput set-prop "$(xinput list | grep -i touchpad | cut -f 2 | grep -oE '[[:digit:]]+')" 'Device Enabled' 1"
alias stickd="xinput set-prop "$(xinput list | grep -i stick | cut -f 2 | grep -oE '[[:digit:]]+')" 'Device Enabled' 0"
alias sticke="xinput set-prop "$(xinput list | grep -i stick | cut -f 2 | grep -oE '[[:digit:]]+')" 'Device Enabled' 1"
# start wpa_supplicant -- sudo systemctl start wpa_supplicant
# wpa_passphrase Frontier4704 21422325889218 > /etc/wpa_supplicant.conf #5148275597
alias wpabg='sudo wpa_supplicant -i wlp1s0 -c f -B'  # (-d for debugging, -B for background/daemon)
alias ipsu='sudo ip link set wlp1s0 up'
# ip a -- get status of wireless devices
alias sudo='sudo '
alias e='exit'
#alias redshift='redshift -l 32.96:-96.67 -t 6500:2000'
#alias lproc='echo "use htop"'
alias ls='ls --color=auto'
alias lst='ps -A | grep'
alias kill='kill -9'
alias mv='mv -i'
alias cp='cp -i'
alias cr='less ~/.nixenv | grep'   # check nixpkgs repo file
alias crd='nix-env -qaP --description | grep'   # check nixpkgs repo
alias update='nix-channel --update'   # check nixpkgs repo
alias rb='sudo nixos-rebuild switch'  # rebuild NixOS
alias eba='vim ~/.bashrc'
alias evi='vim ~/.vimrc'
alias enc='sudo vim /etc/nixos/configuration.nix'
alias eiwm='vim ~/.i3/config'
alias ete='vim ~/.config/termite/config'
alias xr='xrdb -merge ~/.Xdefaults'
alias conh='sudo nmcli device wifi connect Frontier4704 password 21422325889218' #5148275597
alias con='nmcli device wifi connect'
alias wifi='nmcli device wifi'
alias c='clear'
alias ns='nix-shell'
alias xflux='xflux -z 75044'
alias inst='nix-env -iA'
alias unin='nix-env --uninstall'
alias listinst='nix-env -q --installed'
alias bat='acpi'    # or 'upower ...'
alias eis='vim ~/.i3status.conf'
alias v='vim'
alias etm='vim ~/.tmux.conf'
alias est='vim ~/.config/stretchly/config.json'
alias music='cmuse'
alias zip='7z a -tzip'
alias unzip='7z x'
alias pavucontrol='pavucontrol-qt'
alias clean='nix-collect-garbage -d'
alias repo='nix-env -qaP'
alias srm='mv -t /home/cameron/.trash'
alias bhi='sudo brightnessctl set 100%'
alias bmhi='sudo brightnessctl set 75%'
alias bmed='sudo brightnessctl set 50%'
alias bmlo='sudo brightnessctl set 25%'
alias blo='sudo brightnessctl set 10%'

# # git commands
alias com="git commit"
alias st="git status"
alias ch="git checkout"
alias logp="git log --pretty=oneline"
alias br="git branch"
alias pull="git pull"
alias pullr="git pull --rebase"
alias push="git push"
alias tag="git tag"
alias add="git add"
alias ammend="git commit ammend"
alias reba="git rebase"
alias logg="git log --all --decorate --oneline --graph"

# alias anp='nix-env -f '<nixpkgs>' -iA'   # nodePackages.searchterm
# alias es='vim ~/.config/sway/config'


PS1='[\u@\h \W]\$ '

stty -ixon

