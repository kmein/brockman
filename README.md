# Brockman
Read your favourite RSS feeds in your favourite IRC channels

## Usage

### Installation
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

### Configuration
In case you don't want to (or cannot) use the provided NixOS module, you can always simply execute `brockman PATH/TO/CONFIG.json` to fire up an instance of Brockman with a specified config.
For a fully fledged config, look at [the example](./config/example.json).

- `channel` specifies the channel all bots should join and post to.
- `useTls` (optional, default `false`) specifies whether the IRC connection should be made using TLS.
- `shortener` (optional) specifies an endpoint that returns shortened URLs when they are POSTed to it.
- `pastebin` (optional) specifies an endpoint that returns a link when you POST content to it.
- `defaultDelay` (optional) specifies the amount of seconds to wait before fetching each feed again. This is only used when `bot.NAME.delay` isn't set for a specific feed and when Brockman cannot figure out an adequate interval by itself.
- `maxStartDelay` (optional, default `60`) is used for Brockman's startup sequence: if you have many—say, a thousand—bots and start them all at once, you won't get an even stream of news in your channels. Therefore, a bot's startup will be delayed by a random number of seconds. This field specifies the maximum.
- `notifyErrors` (optional, default `true`) specifies whether to notify (IRC notice) of errors that happen when fetching feeds, e. g. 404 errors etc.
- `statePath` (optional, default `$HOME/brockman.json`) specifies the path of Brockman's state file.
- `irc.host` specifies the IRC server to connect to.
- `irc.port` (optional, default `6667`) specifies the port of that IRC server.
- `controller.nick` when set, enables the controller-bot feature: A bot with the specified nick will join the channel specified in `channel` (alongside the channels specified in `controller.extraChannels`). For more information on how to interact with that bot, see [below](#controller).
- `controller.extraChannels` (optional, default `[]`): see above.
- `bots.NAME.feed` initializes a bot named `NAME` with a feed. That bot will automatically join the channel specified in `channel` and post updates there.
- `bots.NAME.delay` (optional) overrides the interval with which the bot checks its feed. This is figured out intelligently if you don't specify a value.
- `bots.NAME.extraChannels` (optional, default `[]`) specifies additional channels, to which the bot posts its messages.

### Interacting with Brockman
Brockman keeps track of its state using a state file that has the same format as the config specified above. While Brockman is running, you can interactively modify its state, e. g. move bots to different channels, override their delays, add new bots. That modified state is regularly written to the specified `statePath`. On startup, Brockman will try to resume from `statePath` if it finds a valid config there.

#### Reporter Bots
Reporter bots (those specified under the `bots` key) can be `/invite`d to and `/kick`ed from channels they are in. On an `/invite NAME` from a channel, that channel will be added to the bot's `extraChannels` and the bot will join said channel. On `/kick NAME`, the current channel name will be removed from the bot's `extraChannels`.

#### Controller
The controller bot can be used to add (`CONTROLLERNAME: add BOTNAME FEEDURL`), modify (`CONTROLLERNAME: tick BOTNAME TICKSECONDS`, `CONTROLLERNAME: set-url BOTNAME FEEDURL`) and remove (`CONTROLLERNAME: remove BOTNAME`) reporter bots. It can also subscribe you (`CONTROLLERNAME: (un)subscribe BOTNAME`) to a bot's message via IRC privmsg.

It can also give information about its current configuration, i. e. which bot watches which feed and how often (`CONTROLLERNAME: info BOTNAME`), which user is subscribed to which bot (`CONTROLLERNAME: info NICK`) etc.

If a `pastebin` is set, `CONTROLLERNAME: dump` will publish the current state online, making it easy for you to fork a running brockman instance.

Like the reporter bots, the controller bot can be `/invite`d and `/kick`ed and will adjust its `extraChannels` value accordingly.

## Development

### Development REPL
- Nix: `nix-shell --attr env default.nix --run "cabal repl"  `

### Build
- Nix: `nix-build`
- Legacy: `cabal build`

### Test
Running [`vm-test/start.sh`](./vm-test/start.sh) (a [Nix](https://nixos.org) installation is required) will fire up a NixOS VM running an IRC server together with a `brockman` instance with one RSS feed, which will post to the `#news` channel.


## Eponym

![Kent Brockman](https://vignette.wikia.nocookie.net/simpsons/images/5/52/Kent_Brockman_2.png/revision/latest?cb=20121228104403&path-prefix=it)
