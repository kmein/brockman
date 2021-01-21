# Brockman
Read RSS feeds in your favourite IRC channels

## Usage

### Development REPL
- Nix: `nix-shell --attr env default.nix --run "cabal repl"  `

### Build
- Nix: `nix-build`
- Legacy: `cabal build`

### Test
Running [`vm-test/start.sh`](./vm-test/start.sh) (a [Nix](https://nixos.org) installation is required) will fire up a NixOS VM running an IRC server together with a `brockman` instance with one RSS feed, which will post to the `#news` channel.

### Install
- Nix (profile): `nix-env -i -f default.nix`
- NixOS
  1. import the module from [`module/default.nix`](./module/default.nix), i. e. add to the `imports` section of your NixOS configuration
  2. add the following option to your configuration

  ```nix
  {
    services.brockman = {
      enable = true;
      config = {
        irc.host = "<IRC_HOST>";
        bots.bchan = {
          feed = "http://boards.4channel.org/b/index.rss";
          channels = [ "#news" ];
          delay = 500; # RSS checking interval
        };
      };
    };
  }
  ```
- Legacy: `cabal install`

## Eponym

![Kent Brockman](https://vignette.wikia.nocookie.net/simpsons/images/5/52/Kent_Brockman_2.png/revision/latest?cb=20121228104403&path-prefix=it)
