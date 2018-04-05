# matrix-dirwatch-bot

[![Build Status](https://travis-ci.org/plapadoo/matrix-dirwatch-bot.svg?branch=master)](https://travis-ci.org/plapadoo/matrix-dirwatch-bot)

This bot listens for changes in a directory and outputs these changes to standard output in a format that can be fed directly to the [matrix-bot](https://github.com/plapadoo/matrix-bot).

## Installation and usage

### Via Docker

The easiest way to install the bot (or just try it out) is via Docker. To start the bot and inspect its output, run:

    docker run -i -v "$watch_dir:$watch_dir":ro --name=matrix-dirwatch-bot --rm "plapadoo/matrix-dirwatch-bot" matrix-dirwatch-exe "--directory=$watch_dir" "--exclude=$exclude"
	
Here, you can replace `$watch_dir` by the directory you want to monitor, and `$exclude` by the files you want to exclude. Executing the command will print out changes in the form suited for passing it to curl. As you can see, the bot is designed by way of the Unix philosophy: It doesn’t handle HTTP, it just handles the monitoring part. It’s your responsibility to pass the information along. 

An example of how to do that is contained in `docs/matrix-dirwatch.sh`, in which `unbuffer`, the bot, `curl` and `xargs` are combined with pipes to feed the data to the matrix-bot.

This process can also be combined with systemd. A sample service is provieded in `docs/matrix-dirwatch.service`.

### Manually

Assuming you have compiled the bot yourself, you’re left with a single executable file:  `matrix-dirwatch-exe`. The command line is the same as in the docker example.

## Compilation from source

### Using Nix

The easiest way to compile the bot or the docker image from source is to use the [nix package manager](https://nixos.org/nix/). With it, you can build the bot using

    nix-build

The resulting files will be located in the `result/` directory. To build the Docker image, use

    nix-build dockerimage.nix

This will, at the last line, output a path that you can feed into `docker load`.

### Using cabal

The bot can be compiled using [cabal-install](https://www.haskell.org/cabal/) by using `cabal install --only-dependencies` and then `cabal install`.
