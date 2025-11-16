---
title: Ad-Hoc Emacs Packages with Nix
summary: Creating ad-hoc Emacs packages in a few lines of code.
---

You can use [Nix][nix] as a package manager for Emacs, like so:

```nix
{
  home-manager.users.eudoxia = {
    programs.emacs = {
      enable = true;
      extraPackages =
        epkgs: with epkgs; [
          magit
          rust-mode
          treemacs
          # and so on
        ];
    };
  };
}
```

I recently learnt you can also use it to create ad-hoc packages for things not
in [MELPA][melpa] or [nixpkgs].

Recently I wanted to get back into [Inform 7][i7], naturally the first step was
to look for an Emacs mode. [`inform7-mode`][i7m] exists, but isn't packaged
anywhere. So I had to vendor it in.

You can use [git submodules][sub] for this, but I hate them. Instead I did
something far more annoying: I wrote a [Makefile] to download the `.el` from
GitHub with `curl`, and used [home-manager][hm] to put copy it into my
`.emacs.d`. Which is nasty. And of course this only works for small, single-file
packages. And there are other problems: whatever dependencies your vendored
packages need have to be specified in `extraPackages`, which confuses the
packages _you_ want directly, with the transitive dependencies of your vendored
packages.

I felt like [the orange juice bit from _The Simpsons_][oj]. There must be a
better way!

And there is. Lo:

```nix
{
  home-manager.users.eudoxia = {
    programs.emacs = {
      enable = true;
      extraPackages =
        epkgs: with epkgs; [
          customPackages.inform7-mode
          # ...
        ];
    };
  };
}
```


Recently I used a Makefile that would download the necessary `.el` from GitHub, then use `home-manager` to copy it into `.emacs.d`, but this only works for simple

The first time I had cause to do this recently was when I wanted to use the
[inform7-mode][i7m] for [Inform 7][i7], which is packaged nowehere.

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

[nix]: https://nixos.org/
[oj]: https://www.youtube.com/watch?v=viejY6UZ5Bk&t=39s
