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
- Avoid multiline lambdas in `map`, `for-each`, etc...
- One import per line to be `M-x sort-lines` friendly
- Avoid shorthand names like `lp`, `kt` instead of `key-table`. In
  general single or two char variable names are dubious, three might
  be fine (e.g. `tmp`).
- 2 space indentation, do not use tabs

## Manual

Use `Ctrl+X Ctrl+C` to quit!

## How to contribute

1. Before coding, create an issue and describe what you want to do and
   how you want to do it. If you don't know how to do it, ask!

2. Then:

    1. `git clone git@github.com:amirouche/zk.git`
    2. Rename `origin` remote to `upstream` with `cd zk && git remote
       rename origin upstream`
    3. Fork the repository
    4. Add your fork as origin of the repository: `git remote add
       origin git@github.com:amirouche/zk.git`
    5. Create a branch with sensible name for the feature you want to
       implement: `git checkout -b interesting-feature`
    6. Push the branch to github with `git push`
    7. Create a pull-request
    8. Now you can commit and push
    9. Ping @amirouche when you feel like it's ready for a review or
       merge

Thanks again for contributing!
