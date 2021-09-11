---
title: Reading a File in OCaml on Windows
summary: Adventures in officially unsupported architectures.
---

The trivial way to read a file into a string in OCaml is this (thanks,
[StackOverflow][so]):

```ocaml
let read_file_to_string (path: string): string =
  let stream = open_in path in
  try
    let len = in_channel_length stream in
    let str = really_input_string stream len in
    close_in stream;
    str
  with _ ->
    close_in_noerr stream;
    raise (Failure ("Failed to read file: " ^ path))
```

But this doesn't work on Windows: `really_input_string` throws `End_of_file`
because the readable length of the file is less than the length attested by
`in_channel_length`. I don't care to debug this. This alternative implementation
works:

```ocaml
let read_file_to_string (path: string): string =
  let rec read_stream stream =
    try
      let line = input_line stream in
      line :: (read_stream stream)
    with End_of_file ->
      []
  in
  let stream = open_in path in
  String.concat "\n" (read_stream stream)
```

This was the one thing preventing the [bootstrapping compiler for Austral][aus] from working on
Windows.

[so]: https://stackoverflow.com/questions/53839695/how-do-i-read-the-entire-content-of-a-given-file-into-a-string
[aus]: https://github.com/austral/austral
