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
something far more annoying: I wrote a [Makefile][make] to download the `.el`
from GitHub with `curl`, and used [home-manager][hm] to put copy it into my
`.emacs.d`. Which is nasty.

And of course this only works for small, single-file packages. And there are
other problems: whatever dependencies your vendored packages need have to be
specified in `extraPackages`, which confuses the packages _you_ want directly,
with the transitive dependencies of your vendored packages.

I felt like [the orange juice bit][oj] from _The Simpsons_. There must be a
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

# xcompose-mode

- xcompose-mode

# eat

Somehow the version of `eat` in nixpkgs unstable was missing the configuration
option to use a custom shell. Since I want to use [nu] instead of bash, I had to
package this myself from the latest commit:

```nix
eat = pkgs.emacsPackages.trivialBuild {
  pname = "eat";
  version = "unstable";
  src = pkgs.fetchgit {
    url = "https://codeberg.org/akib/emacs-eat.git";
    rev = "c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99";
    sha256 = "sha256-9xG2rMlaMFY77JzUQ3JFrc7XKILZSL8TbP/BkzvBvMk=";
  };
  packageRequires = with pkgs.emacsPackages; [
    compat
  ];
};
```

# cabal-mode

- cabal-mode
  - recently created a haskell project with cabal init
  - opening the `.cabal` file i saw there was no syntax highlighting
  - surprisingly, there's no cabal-mode on melpa
  - coincidentally, someone started working on a cabal-mode just 3wk ago!

# lean4-mode

I started reading [_Functional Programming in Lean_][fpil] recently, and while
there is a [lean4-mode][l4m], it's not packaged anywhere. This only required a
slight deviation from the pattern: when I opened a `.lean` file I got an error
about a missing JSON file, consulting the README for `lean4-mode`, it says:

> If you use a source-based package-manager (e.g. `package-vc.el`, Straight or
> Elpaca), then make sure to list the `"data"` directory in your Lean4-Mode
> package recipe.

To do this I had to use `melpaBuild` rather than `trivialBuild`:

```nix
lean4-mode = pkgs.emacsPackages.melpaBuild {
  pname = "lean4-mode";
  version = "1.1.2";
  src = pkgs.fetchFromGitHub {
    owner = "leanprover-community";
    repo = "lean4-mode";
    rev = "1388f9d1429e38a39ab913c6daae55f6ce799479";
    sha256 = "sha256-6XFcyqSTx1CwNWqQvIc25cuQMwh3YXnbgr5cDiOCxBk=";
  };
  packageRequires = with pkgs.emacsPackages; [
    dash
    lsp-mode
    magit-section
  ];
  files = ''("*.el" "data")'';
};
```

[fpil]: https://lean-lang.org/functional_programming_in_lean/
[hm]: https://github.com/nix-community/home-manager
[i7]: https://ganelson.github.io/inform-website/
[i7m]: https://github.com/alexispurslane/inform7-mode
[l4m]: https://github.com/leanprover-community/lean4-mode
[make]: https://en.wikipedia.org/wiki/Make_(software)
[melpa]: https://melpa.org/
[nix]: https://nixos.org/
[nixpkgs]: https://github.com/NixOS/nixpkgs
[nu]: https://www.nushell.sh/
[oj]: https://www.youtube.com/watch?v=viejY6UZ5Bk&t=39s
[sub]: https://git-scm.com/book/en/v2/Git-Tools-Submodules
