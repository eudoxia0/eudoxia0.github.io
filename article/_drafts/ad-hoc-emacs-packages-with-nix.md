---
title: Ad-Hoc Emacs Packages with Nix
summary: Creating ad-hoc Emacs packages in a few lines of code.
---

- use nix as an emacs package manager
  - example from my dotfiles
- you can also use this to create ad-hoc packages for things not in MELPA or nixpkgs
- recently i've had two causes to use this
  - inform7-mode
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
- armed with a new hammer, i set out to stick some nails
  - lean4-mode
    - started reading func prog in lean recently
    - there's a lean4-mode
    - not in melpa
    - this one required a slight deviation from pattern, had to use melpa build
      to copy the data folder which is needed
  - xcompose-mode
  - custom eat
    - somehow the version of eat packages in nixpkgs unstable is missing the config option to change the shell to `nu`
  - cabal-mode
    - recently created a haskell project with cabal init
    - opening the `.cabal` file i saw there was no syntax highlighting
    - surprisingly, there's no cabal-mode on melpa
    - coincidentally, someone started working on a cabal-mode just 3wk ago!
