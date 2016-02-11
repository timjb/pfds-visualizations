# Visualizations of Purely Functional Data Structures

Currently includes visualizations of three queues described in Okasaki's book:

* Banker's Queues
* Amortized Queues
* Real-time queues

# Building

1. Install a recent version of GHCJS:

```bash
$ git clone https://github.com/ghcjs/ghcjs.git
$ cabal install ./ghcjs
$ ghcjs-boot --dev
```

2. Install dependencies and compile Haskell to JavaScript with `stack`:

```bash
$ stack build
```

3. Minify JavaScript files:

```
$ make
```