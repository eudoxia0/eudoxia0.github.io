---
title: Test Autodiscovery is a Silver Bullet
summary: To encourage good practices, make them frictionless.
card: test-autodiscovery-silver-bullet.jpg
card_source: |
    “Cover for Neuromancer, Paul Klee, 1927, watercolor, from Wikipedia”, DALL-E, June 2022.
---

I tend to write a lot more tests in Rust than in [OCaml][ocaml]. Why? Not
because Rust is so unsafe it needs more tests. Rust and OCaml are at roughly
equivalent levels of safety. It's because of ergonomics.

[ocaml]: /article/two-years-ocaml

The [cargo][cargo] developers have done enough work that the _entire_ process
for writing and running a unit test in Rust, from scratch, is:

[cargo]: https://github.com/rust-lang/cargo

```bash
$ cargo new hello-world
$ cd hello-world
$ mkdir tests
$ echo '#[test]
fn test_two_plus_two() {
    assert_eq!(2+2, 4);
}' > tests/test.rs
$ cargo test
running 1 test
test test_two_plus_two ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s
```

That's it. That's all there is to it. And that includes creating a new empty
project, if you're working on an existing codebase, it's even easier: just open
a test module and write a function.

`cargo` will discover the tests automatically, every test is a function that
starts from nothing and returns nothing. Tests are run in parallel. Suites?
Modules are test suites. Setup, teardown? Function calls. If you need a more
sophisticated setup, you can build it.

Compare OCaml: when I was setting up tests for [Austral][aus], it took me a lot
of trial and error and reading Discourse threads and Reddit posts to write the
most basic `assert(true = true)` unit test. `dune new` is useless, you'd better
hope you have a [project starter template][tmpl] that sets up unit testing for
you. And even when you have the test harness set up, writing and running unit
tests is unergonomic:

[aus]: https://github.com/austral/austral
[tmpl]: https://github.com/eudoxia0/ocaml-nix-starter

1. You have to first open a test module, say, `FooTest`.

1. And write a test function:

   ```ocaml
   open OUnit2

   let test_two_plus_two _ =
     assert_equal (2 + 2) 4
   ```

2. And wire it up into a test suite:

   ```ocaml
    let suite =
      "Arithmetic" >::: [
        "two plus two" >:: test_two_plus_two;
      ]
   ```

3. And export a function to run the suite, if you haven't done that:

   ```ocaml
   let _ = run_test_tt_main suite
   ```

4. And register the module in the `dune` file, if it's not already there:

   ```lisp
   (tests
     (names
       FooTest))
    ```

And then, only then, can you run `dune runtest` and start debugging the
inevitable module visibility issues. Consider all the sources of friction:

1. Choosing a unit testing framework.
2. Integrating this into [dune][dune]. Normally this would be an unremarkable
   step, but dune is very painful.
3. Choosing both a name for the test function, and a string description of it.
4. Wiring everything together.

[dune]: https://dune.build/

If there's a better way to do it, it's not widely advertised, and I don't care
to investigate it because getting _this_ setup going was already frustrating
enough.

The result: in theory, OCaml should be safer than Rust, because it's
garbage-collected and high-level and you can write purely-functional code
guilt-free. In practice, my OCaml code is woefully undertested, while my Rust
code is tested every which way. Because in OCaml---not because of the language,
but because of tooling choices---testing is painful, but in Rust it's easy.

Python also gets this right: writing unit tests in Python is one of the few
aspects of the language that is simply delightful.

This ties into [programming language pragmatics][pragma]: discipline doesn't
scale, and if you want people to use good practices, you have to make it
ergonomic to do so.

[pragma]: /article/language-pragmatics
