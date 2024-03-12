# Gerbil-sequentia

First, setup  data directory for node:
```bash
mkdir -p ./data/elementsdir1
cp elements.conf ./data/elementsdir1/elements.conf
```

Then, to run a test scenario: 

```bash
nix-shell
gerbil build && ./scripts/test-scenarios.ss test-any-fee-transaction
```
