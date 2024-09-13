# see configuration.nix(5)

{ config, pkgs, ... }:

{

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      #Minimal config, for headless
      #imports = [ <nixpkgs/nixos/modules/profiles/minimal.nix> ];
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  #boot.loader.grub.devices = "";

  networking = {
    hostName = "nixos"; # Define your hostname.
    wireless.enable = true;  # Enables wireless support via wpa_supplicant
    extraHosts =
      '' #nixos version of /etc/hosts
      '';
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    # i18n = {
    #   consoleFont = "Lat2-Terminus16";
    #   consoleKeyMap = "dvorak";
    #   defaultLocale = "en_US.UTF-8";
    # };

  # Set your time zone.
    time.timeZone = "America/Chicago";


      nixpkgs.config.permittedInsecurePackages = [
        "google-chrome-81.0.4044.138"
        "chromium-81.0.4044.138"
        "chromium-unwrapped-81.0.4044.138"
      ];


  # Load fonts
  fonts.fonts = with pkgs; [
    inconsolata fira-code hasklig hanazono lmmath xits-math stix-two libertinus tex-gyre-bonum-math tex-gyre-schola-math
    corefonts
  ];

  fonts.fontconfig = {
    # ultimate.enable = true; # shiranai, demo maybe good.
    defaultFonts = { #used when current/default font cannot display glyphs
                     #TODO: does this override ~/.config/fontconfig/fonts.conf?
        monospace = [
            "Fira Code"
            "Hasklig"
            "Monospace"
            "Iosevka"
            "XITS Math"
            "STIX Two Math"
            "Libertinus Math"
            "TeX Gyre Bonum Math"
            "TeX Gyre Schola Math"
            #DejaVu Math TeX Gyre  (should already show up in charmap)
        ];
        sansSerif = [
            "Arial"
            "Nimbus Roman"
        ];
        serif = [
            "Nimbus Roman"
            "Times New Roman"
            "HanaMinA"
        ];
    };
  };

  # internationalization
  i18n.inputMethod = {
    enabled = "fcitx";
    fcitx.engines = with pkgs.fcitx-engines; [ mozc ]; #TODO: change mozc?
  };


  nixpkgs.config = {
    # Allow proprietary packages
    allowUnfree = true;
    # Create an alias for the unstable channel
    packageOverrides = pkgs: {
      unstable = import <nixos-unstable> { # pass the nixpkgs config to the unstable alias to ensure `allowUnfree = true;` is propagated:
        config = config.nixpkgs.config;
      };
    };
  };

  environment.systemPackages = ((import ./pkgs.nix) pkgs);

  # for i3-blocks
  environment.pathsToLink = [ "/libexec" ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  ## for redshift, gammastep, etc.
  location = {
    provider = "manual";
    latitude = 32.96;
    longitude = -96.67;
  };
  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # permissions for kmonad
  services.udev.extraRules =
    ''
      # kmonad user access to /dev/uinput
      KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
    '';

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
    sound.enable = true;
    hardware.pulseaudio = {
        enable = true;
        package = pkgs.pulseaudioFull;
    };
    hardware.bluetooth = {
        enable = true;
       # settings = ''
       # [General]
       # Enable=Source,Sink,Media,Socket,Gateway
       # '';
    };

  programs.sway = {
    enable = true;
  };

  # Enable blueman
  services.blueman.enable = true;

  nix.autoOptimiseStore = true;

  # # automate garbage collection.
  nix.gc = {
    automatic = true;
    # dates = "weekly";
    ## keep only the 10 most recent profile generations
    options = "--delete-older-than +10";
  };

  # Enable the X11 windowing system.
    services.xserver = {
        enable = true;
        xkbVariant = "dvp";
        xkbOptions = "caps:shift";
        windowManager.i3.package = pkgs.i3-gaps;
        autorun = false;
        exportConfiguration = true; # link X config to /etc, so that X config is like in most Linux distros
        synaptics = { #affects only after reboot (or maybe logging-out then in again?) maybe same for libinput
            enable = true;
            tapButtons = false;
            vertEdgeScroll = false;
            horizEdgeScroll = false;
            vertTwoFingerScroll = true;
        };
        # desktopManager.default = "none";
    };

    # # Enable PostgreSQL
    # services.postgresql = {
    #   enable = true;
    #   package = pkgs.postgresql_10;
    #   enableTCPIP = true;
    #   authentication = pkgs.lib.mkOverride 10 ''
    #      local all all trust
    #      host all all ::1/128 trust
    #   '';
    #   initialScript = pkgs.writeText "backend-initScript" ''
    #     CREATE ROLE nixcloud WITHashTable LOGIndex PASSWORD 'nixcloud' CREATEDB;
    #     CREATE DATABASE nixcloud;
    #     GRANT ALL PRIVILEGES ON DATABASE nixcloud TO nixcloud;
    #   '';
    # };


  # Enable touchpad support.

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

virtualisation.virtualbox.host.enable = true;
users.extraGroups.vboxusers.members = [ "cameron" ];

    # Define a user account. Remember to set a password with ‘passwd’.
    users.users.cameron = {
      #shell = pkgs.dash;
      isNormalUser = true;
      group = "cameron";
      extraGroups = [ "wheel" "sway" "fuse" ];
    };

    users.groups = {uinput = {}; };
    users.extraUsers.cameron = {
        extraGroups = [ "input" "uinput" "video" ];
    };

    security.sudo.extraRules = [
      # Allow execution of brightnessctl & rfkill without a password
      { users = ["cameron"];
        commands = [ { command = "brightnessctl";
                       options = [ "SETENV" "NOPASSWD" ]; } ]; } ];


  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "21.05"; # Did you read the comment?

}
