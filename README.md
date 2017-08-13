# Lemming pants

The purpose of this project is to develop a system to manage the speaker list of Datateknologsektionens meetings. Preferably in the simplest possible way.

Feel free to fork, do pull requests and post issues!

The name is much inspired by DHack's totem animal, the [lemming](https://www.youtube.com/watch?v=9A6vm92R9oU).

## Installation

The package is built using either stack or nix and cabal. Please file an issue if you have any problem building this package.

### stack

```bash
stack install
```

### nix + cabal

This is a nice command for starting the nix shell. In the shell you can run `cabal build` and `cabal install` as usual.

```bash
nix-shell --attr env release0.nix
```

When you change anything in the cabal file, run this command:

```bash
cabal2nix . > default.nix
```

TODO: More build guide for nix!
