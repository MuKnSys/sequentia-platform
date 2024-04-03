# sequentia-platform

Test scripts and development tools for [SEQ-Core-Elements](https://github.com/SequentiaSEQ/SEQ-Core-Elements).

## Building

Enter nix shell, which will provide all the build tools and dependencies for both `SEQ-Core-Elements` and the test scripts, written in [Gerbil Scheme](https://gerbil.scheme.org/):
```shell
nix-shell
```
To build `SEQ-Core-Elements`, first import the source code from the submodule by running:
```shell
git submodule update --init --recursive
```
Then run:
```shell
make build-sequentia
```
To build the Gerbil modules required for testing, run:
```shell
gerbil build
```
When making changes to `SEQ-Core-Elements` source, use `make rebuild-sequentia` to avoid recompiling from scratch every time.

## Testing

All test scenarios will spin up a new node, then destroy it immediately after if the test passes successfully. If the test fails, the node will continue running so that its state is still available for debugging purposes.

The scenarios are defined in `./scripts/test.ss`, and list of all the currently available tests can be viewed by running `./scripts/test.ss help`. To run a specific test, add an argument with the name of the test.

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
