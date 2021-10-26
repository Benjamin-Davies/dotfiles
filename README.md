# My Dotfiles

Config files for CLI tools and some GUI stuff

## Dependencies

In order to install and use my dotfiles, your will need at least the following:

* macOS, Linux or Termux (terminal emulator for Android)
* Git (required regardless of how the repo is downloaded)
* CMake
* Make or Ninja
* A C compiler that is recognised by CMake
* Zsh

## Usage

My dotfiles that I want synced are all stored in a central repo and them sym-linked into my home directory.
They are split into the following categories (listed in `categories.txt`):

* devel
* linux (default on Linux)
* macos (default on macOS)
* shell (default on all platforms)
* tiling-wm

To initialise the default setup for your system:

```sh
# Change to your home directory
cd

# Clone the repo
git clone https://github.com/Benjamin-Davies/dotfiles.git
cd dotfiles

# Remove my ssh config (specific to my devices and authorises my keys)
rm -rf shell/ssh/

# Compile scripts, link files and restart the shell
# Note that this may overwrite existing config files
source init
```

From there you can link or unlink additional categories using `cfg-link` and `cfg-unlink` respectively
(these should now be in your path).

You can update the dotfiles using `cfg-update`.
To uninstall you should just be able to just run `cfg-unlink` by itself.
