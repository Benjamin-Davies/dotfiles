# Benjamin Davies' dotfiles

Config files for CLI tools and some GUI stuff

## Getting started

I use [chezmoi](https://www.chezmoi.io/) (pronounced "shay mwa", French for "at my house") to manage my dotfiles. If you already have it installed, then run:

```sh
chezmoi init --apply Benjamin-Davies
```

...otherwise run:

```sh
sh -c "$(curl -fsLS get.chezmoi.io)" -- -b $HOME/.local/bin init --apply Benjamin-Davies
```
