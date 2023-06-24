---
title: A Concise Introduction to GnuPG
---

[GNU Privacy Guard][gnupg], or GnuPG, is a cryptography tool. It lets you
encrypt and decrypt files, send and receive encrypted mail, and digitally sign
messages and verify message signatures. Despite its usefulness it's not widely
used: if you're on Linux you've likely typed a `gpg --recv-keys` command without
understanding what it does. It's reputed to be difficult to use correctly. So I
decided to sit down and learn it, and write down what I learned.

[gnupg]: https://www.gnupg.org/

# Contents

1. [Concepts and Terminology](#concepts)
1. [Installing GnuPG on Nix](#nix)
1. [Key Management](#mgmt)
    1. [Generating a Key](#gen)
    1. [Listing Keys](#list)
    1. [Exporting Public Keys](#export)
    1. [Importing Public Keys](#import)
    1. [Backup and Restore](#backup)
    1. [Key Revocation](#revoke)
1. [Encryption](#crypt)
    1. [Encrypting a File](#encrypt)
    1. [Decrypting a File](#decrypt)
1. [Digital Signatures](#signatures)
    1. [Signing a Message](#sign)
    1. [Verifying a Message](#verify)
1. [See Also](#see-also)

