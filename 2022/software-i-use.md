---
date: 2022-10-07
tags: personal observations
---

# Software I use on my MacBook

Recently, I switched to [Home Manager][`home-manager`] to [manage](https://github.com/abhin4v/nix-managed-macbook)
most of the software I use on my MacBook. This caused to me review all the software I have installed,
so I made this list.

## CLI/TUI software

[`bash`](https://www.gnu.org/software/bash/)
: Run all Bash scripts that'll never go extinct.

[`bat`](https://github.com/sharkdp/bat)
: A better `cat`.

[`broot`](https://dystroy.org/broot/)
: A better `tree`.

[`cloc`](https://github.com/AlDanial/cloc)
: Count lines of code.

[`cloudflare-dyndns`](https://github.com/kissgyorgy/cloudflare-dyndns)
: Dynamic DNS script for updating CloudFlare DNS A records. I use it to point [NextDNS] to my home
network.

[`comma`](https://github.com/Shopify/comma)
: Run software without installing it. Backed by [Nix][`nix`].

[`coreutils`](https://www.gnu.org/software/coreutils/)
: Utilities to write scripts that work same on macOS and Linux.

[`ddgr`](https://github.com/jarun/ddgr)
: Search [DuckDuckGo](https://duckduckgo.com/) from the terminal.

[`delta`](https://github.com/dandavison/delta)
: Syntax highlighting for git, diff and grep.

[`direnv`](https://direnv.net)
: An extension for shells to load and unload environment variables depending on the current directory.

[`dua`](https://github.com/Byron/dua-cli)
: A disk usage analysis tool.

[`entr`](http://eradman.com/entrproject/)
: Run arbitrary commands when files change.

[`exa`](https://the.exa.website)
: A better `ls` (and more colorful too).

[`fd`](https://github.com/sharkdp/fd)
: A better `find`.

[`fish`](https://fishshell.com)
: A modern shell.

[`fzf`](https://github.com/junegunn/fzf)
: A command-line fuzzy finder.

[`git`]
: A distributed version control system.

[`gitui`](https://github.com/Extrawurst/gitui)
: A terminal-UI for [`git`].

[`graphviz`](https://graphviz.org)
: A graph visualization software.

[`home-manager`]
: A [`nix`] based declarative configuration of software and their configs.

[`htop`](https://htop.dev)
: A process viewer in terminal.

[`httpie`](https://httpie.io)
: A better `curl`.

[`just`](https://just.systems)
: A modern command runner.

[`micro`](https://micro-editor.github.io)
: A terminal based editor. A better `nano`.

[`neofetch`](https://github.com/dylanaraps/neofetch)
: A system information tool.

[`nix`](https://nixos.org)
: Nix is a package manager, a system configurator, a development environment provider, and [many other things](https://web.archive.org/web/3/https://www.haskellforall.com/2022/08/stop-calling-everything-nix.html).

[`nixfmt`](https://github.com/serokell/nixfmt)
: A formatter for Nix files.

[`nix-index`](https://github.com/bennofs/nix-index)
: A command-not-found helper using [Nix][`nix`].

[`proselint`](http://proselint.com)
: A linter for prose.

[`ranger`](https://ranger.github.io)
: A terminal file manager.

[`shellcheck`](https://www.shellcheck.net):
: A linter for shell scripts.

[`spotify-tui`](https://github.com/Rigellute/spotify-tui)
: [Spotify] TUI for people like me who are too lazy to leave the terminal.

[`starship`](https://starship.rs)
: A fast and customizable prompt for shells.

[`statix`](https://git.peppe.rs/languages/statix/about/)
: A linter for [Nix][`nix`] files.

[`tealdeer`](https://dbrgn.github.io/tealdeer/)
: Simplified, example based and community-driven man pages.

[`thefuck`](https://github.com/nvbn/thefuck)
: The Fuck is a magnificent app that corrects errors in previous console commands.

## GUI software

[1Password](https://1password.com)
: Password manager.

[Amphetamine](https://apps.apple.com/app/amphetamine/id937984704?mt=12)
: Keep the MacBook awake.

[Android File Transfer](https://www.android.com/filetransfer/)
: Transfer files from MacBooks to Android phones.

[AppCleaner](https://freemacsoft.net/appcleaner/)
: Thoroughly uninstall macOS apps.

[Apple Books](https://www.apple.com/apple-books/)
: Read e-books on macOS.

[Apple Notes](https://apps.apple.com/us/app/notes/id1110145109)
: Note taking.

[Calibre](https://calibre-ebook.com)
: E-book management.

[Dropbox](http://dropbox.com)
: Cloud file storage and syncing.

[Fanny](https://fannywidget.com)
: Menu bar application to monitor CPU temperatures and fan speeds.

[Firefox](https://firefox.com)
: The best browser.

[Garmin Express](https://www.garmin.com/en-US/software/express/)
: Sync/update Garmin devices.

[Google Chrome](https://www.google.com/chrome/)
: The other browser.

[HandBrake](https://handbrake.fr)
: Transcode video.

[IntelliJ IDEA CE](https://www.jetbrains.com/idea/)
: For when I need to write Java code.

[iTerm2](https://iterm2.com)
: The better terminal for macOS.

[Kindle](https://apps.apple.com/us/app/kindle/id405399194?mt=12)
: Read Kindle books on macOS.

[Monodraw](https://monodraw.helftone.com)
: An ASCII art editor for macOS.

[Rectangle](https://rectangleapp.com)
: A window manager for macOS.

[Spotify]
: Stream all the music.

[Telegram](https://desktop.telegram.org/)
: Chatting and instant messaging.

[Visual Studio Code](https://code.visualstudio.com/)
: My preferred code editor.

[VLC](https://www.videolan.org/)
: My preferred video player.

## Haskell specific software

[`cabal-plan`](https://github.com/haskell-hvr/cabal-plan)
: Library-level dependency visualizer for Haskell projects.

[`graphmod`](https://github.com/yav/graphmod)
: Module-level dependency visualizer for Haskell projects.

[`haskell-language-server`](https://github.com/haskell/haskell-language-server)
: Code completion/navigation and intellisense for Haskell.

[`hlint`](https://github.com/ndmitchell/hlint)
: Haskell linter.

[`ormolu`](https://github.com/tweag/ormolu)
: Haskell formatter.

[`stan`](https://kowainik.github.io/projects/stan)
: Haskell source code analyzer.

## Monospace fonts

- [DM Mono](https://fonts.google.com/specimen/DM+Mono)
- [Fira Mono](https://fonts.google.com/specimen/Fira+Mono)
- [Inconsolata](https://fonts.google.com/specimen/Inconsolata)
- [Jetbrains Mono](https://fonts.google.com/specimen/JetBrains+Mono)
- [Nanum Gothic Coding](https://fonts.google.com/specimen/Nanum+Gothic+Coding)
- [Roboto Mono](https://fonts.google.com/specimen/Roboto+Mono)
- [Source Code Pro](https://fonts.google.com/specimen/Source+Code+Pro)
- [Nerdfonts](https://www.nerdfonts.com/)
  - [Monoid](https://www.programmingfonts.org/#monoid)
  - [Agave](https://www.programmingfonts.org/#agave)
  - [Iosevka](https://www.programmingfonts.org/#iosevka)
  - [Lekton](https://www.programmingfonts.org/#lekton)
  - [VictorMono](https://www.programmingfonts.org/#victor-mono)

[`home-manager`]: https://github.com/nix-community/home-manager/
[`nix`]: https://nixos.org
[`git`]: https://git-scm.com
[NextDNS]: https://nextdns.io/?from=7ax2p5e9
[Spotify]: https://spotify.com
