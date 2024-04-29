# sequentia-platform

This repository contains development tools and test scripts for
[SEQ-Core-Elements](https://github.com/SequentiaSEQ/SEQ-Core-Elements).

This file explains how to set it all up.


## Overview

We build the software deterministically using [Nix](https://nixos.org/),
ensuring that it will run the very same on anyone's machine.

## Building all the software

### Installing Nix

If you aren't using Nix yet, install it from the
[Nix download page](https://nixos.org/download/).

On Linux (or Windows using WSL), this is typically as follows
(on macOS, omit the `--daemon`):
```
sh <(curl -L https://nixos.org/nix/install) --daemon
```

Another option is the
[Determinate Nix Installer](https://determinate.systems/posts/determinate-nix-installer/).

### Configuring Nix

Once you installed Nix, you can configure it to use our pre-compiled packages
instead of recompiling everything from source,
by creating or editing your `~/.config/nix/nix.conf` and adding these lines:
```
substituters = https://cache.nixos.org https://cache.nixos.org/ https://mukn.cachix.org
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= mukn.cachix.org-1:ujoZLZMpGNQMeZbLBxmOcO7aj+7E5XSnZxwFpuhhsqs=
```

### Using a Nix shell

You may now enter a nix shell, which will provide all the build tools and dependencies
for both `SEQ-Core-Elements` and the tools and test scripts,
written in [Gerbil Scheme](https://cons.io/):
```shell
nix-shell
```

### Checking out Sequentia source

To build `SEQ-Core-Elements`, first clone the repository
into the parent directory of this repo:
```shell
git clone git@github.com:SequentiaSEQ/SEQ-Core-Elements.git ../SEQ-Core-Elements
```

Furthermore you may want to use the `dev` branch for the latest development:
```shell
(cd ../SEQ-Core-Elements; git checkout dev)
```

You may independently be developing the Sequentia Core software from that directory,
using whichever branch you need, all from within your nix shell.

### Building Sequentia Core source

Then, still within your nix shell, run:
```shell
make build-sequentia
```

When making changes to `SEQ-Core-Elements` source,
use `make rebuild-sequentia` to avoid recompiling from scratch every time.

### Building Sequentia platform tooks

To build the Gerbil modules required for testing, run:
```shell
gerbil build
```

## Testing

All test scenarios will spin up a new node,
then destroy it immediately after if the test passes successfully.
If the test fails, the node will continue running
so that its state is still available for debugging purposes.

The scenarios are defined in `./scripts/test.ss`.
The list of all the currently available tests can be viewed by running
`./scripts/test.ss help`.
To run a specific test, add the name of the test as argument.

For example, to run the test scenario for the No Coin feature:
```shell
./scripts/test.ss no-coin-transaction
```

## Debugging

To build `SEQ-Core-Elements` with debugging enabled, run:
```shell
make build-sequentia-debug
```
Then use `make rebuild-sequentia` when making changes to the local source.
