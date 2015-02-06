{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/vda";

  networking.hostId = "2bc28922";

  i18n = {
    consoleFont="lat9w-16";
    consoleKeyMap="us";
    defaultLocale="en_US.UTF-8";
  };

  services.openssh.enable = true;
  security.sudo.enable = true;

  users.extraUsers.ben = {
    isNormalUser = true;
    uid = 1000;
    group = "users";
    extraGroups = ["wheel"];
  };
}
