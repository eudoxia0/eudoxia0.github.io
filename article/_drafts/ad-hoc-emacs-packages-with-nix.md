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

Recently I wanted to get back into [Inform 7][i7], naturally the first stack
frame of the yak shave was to look for an Emacs mode. [`inform7-mode`][i7m]
exists, but isn't packaged anywhere. So I had to vendor it in.

You can use [git submodules][sub] for this, but I hate them. Instead I did
something far more annoying: I wrote a [Makefile] to download the `.el` from
GitHub with `curl`, and used [home-manager][hm] to put copy it into my
`.emacs.d`. Which is nasty.

And of course this only works for small, single-file packages. And there are
other problems: whatever dependencies your vendored packages need have to be
specified in `extraPackages`, which confuses the packages _you_ want directly,
with the transitive dependencies of your vendored packages.

I felt like [the orange juice bit from _The Simpsons_][oj]. There must be a
better way!

And there is. With some help from Claude, I wrote this:

```nix
let
  customPackages = {
    inform7-mode = pkgs.emacsPackages.trivialBuild {
      pname = "inform7-mode";
      version = "unstable";
      src = pkgs.fetchFromGitHub {
        owner = "alexispurslane";
        repo = "inform7-mode";
        rev = "f99e534768c816ec038f34126f88d816c2f7d9ff";
        sha256 = "sha256-r9Zzd8Ro3p+Bae11bf1WIeVWkbmg17RKLDqG4UcFT1o=";
      };
      packageRequires = with pkgs.emacsPackages; [
        s
      ];
    };
  };
in
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

Nix takes care of everything: commit pinning, security (with the SHA-256 hash),
dependencies for custom packages. And it works wonderfully.

Armed with a new hammer, I set out to sink some nails.

# lean4-mode

- lean4-mode
  - started reading func prog in lean recently
  - there's a lean4-mode
  - not in melpa
  - this one required a slight deviation from pattern, had to use melpa build
    to copy the data folder which is needed


# xcompose-mode

- xcompose-mode

# eat

- custom eat
  - somehow the version of eat packages in nixpkgs unstable is missing the config option to change the shell to `nu`

# cabal-mode

- cabal-mode
  - recently created a haskell project with cabal init
  - opening the `.cabal` file i saw there was no syntax highlighting
  - surprisingly, there's no cabal-mode on melpa
  - coincidentally, someone started working on a cabal-mode just 3wk ago!

[nix]: https://nixos.org/
[oj]: https://www.youtube.com/watch?v=viejY6UZ5Bk&t=39s
