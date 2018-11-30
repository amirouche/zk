# zk - terminal text editor

## Getting started

You need [chez scheme](https://github.com/cisco/chezscheme). It's
available on Ubuntu 18.04 as `chezscheme` but the development happens
against `git` version. You will also need [akku](http://akkuscm.org/).

On ubuntu, you can try the following to install the system
dependencies:

```
sudo apt install build-essential git python chezscheme
```

Then install [akku](https://akkuscm.org/) and don't forget to add it
to your `$PATH`.

Once you have scheme, I assume you have git, you can proceed with the
following:

```bash
$ git clone git@github.com:amirouche/zk.git
$ cd zk
$ make dev
```

## [ROADMAP](https://github.com/amirouche/zk/issues/3)

## Style guide

- Add docstring, at least, to exported procedures
- Avoid multiline lambda in `map`, `for-each`, etc...
- One import per line to be `M-x sort-lines` friendly
- Avoid shorthand names like `lp`, `kt` instead of `key-table`. In
  general single or two char variable names are dubious, three might
  be fine (e.g. `tmp`).
- 2 space indentation, do not use tabs

## Manual

Use `Ctrl+Q` to quit!
