{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/vda";

  networking.hostId = "2bc28922";
  networking.firewall.allowedUDPPorts = [60001]; # For Mosh
  networking.firewall.allowPing = true;

  i18n = {
    consoleFont="lat9w-16";
    consoleKeyMap="us";
    defaultLocale="en_US.UTF-8";
  };

  environment.systemPackages = with pkgs; [
    wget curl mosh
  ];

  services.openssh.enable = true;
  services.openssh.permitRootLogin = "no";

  security.sudo.enable = true;

  users.extraUsers.ben = {
    isNormalUser = true;
    uid = 1000;
    group = "users";
    extraGroups = ["wheel"];
    openssh.authorizedKeys.keys =
      [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDs7c+tW9nfevShFfHyNWzsl/wXYKZh8UoXBOOyCoDLa8blwrLFmTOEOJY2hdX58YlLSJbYvponUMQOJoCUrlS1Sj3CH+MGR9e/XzS0+js4QtzTvof22qUB4wqyeCHD9XRDR1YHgOfiIeS6XyQFqMuTNh5/rKTPff5bjWpfcMRa+aGcLRcE9FYDYIjeqVOMWYL+oPTEr5c4I9MXoiJMZfOfz57pHHpsnBrQawSW1WV5xqOeFZFZiNyGHPyWTKrTjIgVC9bY0CYqiOM9wc6VJtuzz+BiyDlpPaHlBJNd6JvSsqkIBeb4etXyWeEpfNbd20LE68NDoqd0/AJWCioPG39P b@benjamins-air" ];
  };
}
