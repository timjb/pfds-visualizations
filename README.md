# [Visualizations of Purely Functional Data Structures](http://timbaumann.info/pfds-visualizations/)

Currently includes visualizations of three queues described in Okasaki's book:

* Banker's Queues
* Amortized Queues
* Real-time queues

I've created these visualizations for a talk about purely functional queues ([video](https://www.youtube.com/watch?v=u14K7z365mY), [slides](http://timbaumann.info/presentations/bankers-queue/bankers-queue.pdf)) at [Curry Club Augsburg](http://curry-club-augsburg.de).

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
