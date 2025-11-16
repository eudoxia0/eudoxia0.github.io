---
title: Ad-Hoc Emacs Packages with Nix
---

- use nix as an emacs package manager
  - example from my dotfiles
- you can also use this to create ad-hoc packages for things not in MELPA or nixpkgs
- recently i've had two causes to use this
  - inform7-mode
  - xcompose-mode
- my normal solution to this would be vendoring them in
  - you can use submodules for this
  - but i hate submodules
  - so originally i wrote a makefile to download the source files, and used home-manager to copy them to my .emacs.d
  - but this is nasty
    - vendoring is awful
    - whatever dependencies those packages need have to go in your own emacs
      package list, confusing the packages you need yourself vs transitive
      dependencies
- with some help from claude, i wrote a nix expression to create an ad-hoc package for inform7-mode
- and it works wonderfully!
-
