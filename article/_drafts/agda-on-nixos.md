---
title: Agda on NixOS
summary: Troubleshooting notes.
card: agda-on-nixos.webp
card_source: |
    _Olive Trees, Corfu_, [John Singer Sargent][jss], 1909.

    [jss]: https://en.wikipedia.org/wiki/John_Singer_Sargent
---

To install [Agda][agda] and its standard library, add this to your config:

```nix
environment.systemPackages = with pkgs; [
  (agda.withPackages (p: [
      p.standard-library
  ]))
];
```

Or, using [home-manager][hm]:

```nix
home-manager.users.$username.home.packages = with pkgs; [
  (agda.withPackages (p: [
	p.standard-library
  ]))
];
```

The `p` here stands for the `nixpkgs.agdaPackages` package set. Note that the
following _will not_ work:

```nix
environment.systemPackages = with pkgs; [
  agda
  agdaPackages.standard-library
];
```

If you use Emacs, you probably want [`agda2-mode`][mode], which if you use
`home-manager`, can be installed using Nix:

```nix
home-manager.users.eudoxia = {
  programs.emacs = {
    enable = true;
    extraPackages =
      epkgs: with epkgs; [
	    agda2-mode
		# ...
      ];
  };
};
```

Now, if you have a file `hello.agda` with:

```agda
module hello where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.String using (String)

postulate putStrLn : String → IO ⊤
{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# COMPILE GHC putStrLn = putStrLn . T.unpack #-}

main : IO ⊤
main = putStrLn "Hello world!"
```

Then:

```shell
$ agda hello.agda # typecheck
Checking hello (/home/eudoxia/agda/t2/hello.agda).

$ agda -c hello.agda # compile
[snip]
[6 of 6] Linking /home/eudoxia/agda/t2/hello [Objects changed]
$ ./hello
Hello world!
```

So far, so good. Using the standard library however is more complicated. If you
have a file `vec.agda` with:

```agda
module vec where

open import Data.Nat using (ℕ; zero; suc)

data Vec (A : Set) : ℕ → Set where
  []  : Vec A zero
  _∷_ : ∀ {n} (x : A) (xs : Vec A n) → Vec A (suc n)

infixr 5 _∷_
```

Then `agda vec.agda` will not work:

```shell
$ agda vec.agda
Checking vec (/home/eudoxia/agda/t3/vec.agda).
/home/eudoxia/agda/t3/vec.agda:3.1-42: error: [FileNotFound]
Failed to find source of module Data.Nat in any of the following
locations:
  /home/eudoxia/agda/t3/Data/Nat.agda
  /home/eudoxia/agda/t3/Data/Nat.lagda
  /nix/store/[snip]/lib/prim/Data/Nat.agda
  /nix/store/[snip]/lib/prim/Data/Nat.lagda
when scope checking the declaration
  open import Data.Nat using (ℕ; zero; suc)
```

Instead, create a file `vec.agda-lib` in the same directory with:

```
depend: standard-library
include: .
```

Now `agda vec.agda` will succeed.

[agda]: https://en.wikipedia.org/wiki/Agda_(programming_language)
[hm]: https://github.com/nix-community/home-manager
[mode]: https://agda.readthedocs.io/en/stable/tools/emacs-mode.html
