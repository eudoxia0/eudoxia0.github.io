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

# Concepts and Terminology {#concepts}

**Plaintext** is data that has not been encrypted. **Ciphertext** is data that
has been encrypted. A **cipher** is an algorithm that performs encryption or
decryption. **Encryption** uses an encryption cipher to turn plain and a
**cryptographic key** into ciphertext. **Decryption** goes from ciphertext and a
decryption key to plaintext using a decryption cipher.

There are two kinds of encryption: **symmetric encryption** uses a single
**symmetric key** to encrypt data and decrypt data. The most widely used
symmetric encryption scheme is the **Advanced Encryption Standard** (AES), also
known at **Rijndael**. In **assymetric encryption**, or **public-key
encryption**, you have two related keys, a **public key** and a **private key**
forming a **keypair**. The public key can encrypt, the private key can
decrypt. As the names imply: the public key is meant to be shared publicly, the
private key is a closely-guarded secret.

Public-key cryptography has two applications:

1. **Communication:** I can encrypt an email with your public key and send you
   the ciphertext, and only you can decrypt it using your private key. In this
   scheme, the public key of the keypair can encrypt but not decrypt, the
   private key can decrypt but not encrypt. In turn, for me to reply securely, I
   have to encrypt my message with _your_ public key.

2. **Digital Signatures:** I can **sign** a message using a private key, the
   message + **signature** can then be **verified** by anyone with the
   corresponding public key. This is a way of cryptograpically saying, "I wrote
   this".

Why use public-key cryptography and bother with keypairs? The drawback of
symmetric encryption, for communication and signing, is **key exchange**: both
parties have to have the same key, which means the key must be exchanged
securely in some way. Establishing the secure channel is difficult. Public-key
cryptography removes this problem.

A key concept in cryptography is **Kerckhoffs's principle**: a cryptosystem must
be secure if _everything about it is known_ but the private key. Therefore a
good cryptosystem is one that does not rely on secrecy of the algorithm or
implementation.

# Installing GnuPG on Nix {#nix}

If you're using [NixOS][nix], add `gnupg` to the list of packages:

[nix]: https://nixos.org/

```nix
users.users.eudoxia.packages = with pkgs; [
  gnupg
  # ...
];
```

And enable the agent:

```nix
# GnuPG agent.
programs.gnupg.agent = {
   enable = true;
   pinentryFlavor = "curses"; # or `tty`, `gtk2`
};
```

# Key Management {#mgmt}

This section describes the lifecycle of keys: creating them, backing them up,
sharing them, and revoking them.

## Generating a Key {#gen}

Run:

```bash
$ gpg --full-generate-key --expert
```

You will be prompted for a key type:

```
Please select what kind of key you want:
   (1) RSA and RSA
   (2) DSA and Elgamal
   (3) DSA (sign only)
   (4) RSA (sign only)
   (7) DSA (set your own capabilities)
   (8) RSA (set your own capabilities)
   (9) ECC (sign and encrypt) *default*
  (10) ECC (sign only)
  (11) ECC (set your own capabilities)
  (13) Existing key
  (14) Existing key from card
Your selection?
```

The default is correct: ECC is newer and safer than RSA. Select "ECC (sign and
encrypt)" by entering the number and pressing enter. You will then be prompted
by a curve:

```
Please select which elliptic curve you want:
   (1) Curve 25519 *default*
   (2) Curve 448
   (3) NIST P-256
   (4) NIST P-384
   (5) NIST P-521
   (6) Brainpool P-256
   (7) Brainpool P-384
   (8) Brainpool P-512
   (9) secp256k1
Your selection?
```

Again, we want the default, "Curve 25519". You will then be prompted for the
expiry:

```
Please specify how long the key should be valid.
         0 = key does not expire
      <n>  = key expires in n days
      <n>w = key expires in n weeks
      <n>m = key expires in n months
      <n>y = key expires in n years
Key is valid for? (0)
```

I choose `5y` and press enter, and confirm the expiry date:

```
Key expires at Thu 22 Jun 2028 10:26:58 AM AEST
Is this correct? (y/N) y
```

You will then be prompted for your name, email address, and comment. The comment
field is to disambiguate multiple keys belonging to the same person. For
example, you might have one key for email correspondence, another for signing
software releases, and another for signing encrypted backups.

```
Real name: Fernando Borretti
Email address: fernando@borretti.me
Comment: Email
You selected this USER-ID:
    "Fernando Borretti (Email) <fernando@borretti.me>"

Change (N)ame, (C)omment, (E)mail or (O)kay/(Q)uit? O
```

After confirming your choices, you will be asked to enter a passphrase to unlock
the key. Afterwards, the key is generated, and you get an inscrutable block of
text like this:

```
[snip]
```

Let's break this down line by line:

1. `pub` is the key type, it means this is a public key (the private key was
   also generated, but is not shown).
  - `ed25519` means we're using Curve 25519 as intended.
  - `2023-06-24` is when the key was created.
  - `[SC]` lists the key's capabilities: `S` for Sign means we can sign files,
    `C` for certify means we can create certificates.
  - `[expires: 2028-06-22]` is self-explanatory.
2. The ID on the second line is the **fingerprint** of the public key. This is a
   hash of the public key itself, and it is represented as 40 hexadecimal
   characters.
3. `uid` is short for `USER-ID`, this is your name, comment, and email.
4. `sub` indicates a subkey, in this case, the subkey is for encryption. This
   subkey uses `cv25519` as intended, the capabilities are just `[E]` for
   Encrypt, it expires the same day as the public key.

## Listing Keys {#list}

To list your public keys, run:

```bash
$ gpg --list-public-keys --keyid-format=long
[snip]

```

This is basically the output we get when generating the key, with some differences:

1. `pub   [snip]`
   - The string `[snip]` is the short key ID of the public key. This
     is the last 16 characters of the full key ID for the public key.
1. `[snip]`:
   - The fingerprint of the public key. Note that the last 16 characters are the
     same as the short key ID on line one.
2. `sub [snip]`: this shows `[snip]` is the key ID
   of the signing subkey.
   - The short key ID of the signing subkey.

Analogously:

```bash
$ gpg --list-secret-keys --keyid-format=long
```

Shows:

```
[snip]```

The only thing to note here is the fingerprint is the fingerprint of the
corresponding public key, not of the private key.

## Exporting Public Keys {#export}

To let people send you messages with a public key, you need to give it to them
in some way. You do this by exporting the key.

The general form is:

```bash
$ gpg --export --armor <key fingerprint, key ID, or user ID>
```

For the key we just created:

```bash
$ gpg --export --armor [snip]
[snip]
```

This key is in "ASCII Armor" format, a way of representing the (binary) key data
as ASCII text. If you're writing this to a file, the file extension is `.asc`.

The following invocations are all equivalent:

```bash
gpg --export --armor [snip]
gpg --export --armor [snip]
gpg --export --armor [snip]
gpg --export --armor "Fernando Borretti (Email) <fernando@borretti.me>"
```

Exporting the private key lets you back it up.

The general form is:

```bash
$ gpg --export-secret-keys --armor <key fingerprint, key ID, or user ID>
```

For our example key:

```bash
$ gpg --export-secret-keys --armor [snip]
```

## Importing Public Keys {#import}

## Backup and Restore {#backup}

## Key Revocation {#revoke}

# Encryption {#crypt}

## Encrypting a File {#encrypt}

## Decrypting a File {#decrypt}

# Digital Signatures {#signatures}

## Signing a Message {#sign}

## Verifying a Message {#verify}

# See Also {#see-also}

- [GnuPG page on the Arch Linux wiki](https://wiki.archlinux.org/title/GnuPG)
- [GnuPG FAQ](https://www.gnupg.org/faq/gnupg-faq.html)