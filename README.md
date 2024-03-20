# gerbil-sequentia

Test scripts and development tools for [SEQ-Core-Elements](https://github.com/SequentiaSEQ/SEQ-Core-Elements).

## Building

Enter nix shell, which will provide all the build dependencies necessary for both `SEQ-Core-Elements` and `gerbil-sequentia`:
```shell
nix-shell
```
To build `sequentia`, first import the source code from the submodule `SEQ-Core-Elements` by running:
```shell
git submodule update --init --recursive
```
Then run:
```shell
make build-sequentia
```
To build `gerbil-sequentia`, run:
```shell
gerbil build
```

### Testing

All test scenarios will spin up a new node, then destroy it immediately after if the test passes successfully. If the test fails, the node will continue running so that its state is still available for debugging purposes.

The scenarios are defined in `./scripts/test-scenarios.ss`, and list of all the currently dtests can be viewed by running `./scripts/test-scenarios.ss help`. To run a specific test, add an argument with the name of the test.

For example, to run the test scenario for the No Coin feature:
```shell
./scripts/test-scenarios.ss no-coin-transaction
```
