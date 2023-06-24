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

Public-key cryptography is computationally more expensive than symmetric
cryptography. This gave rise to **hybrid ciphers**, which combine both, using
public-key cryptography to establish the secure channel for exchanging a
disposable, one-time-use symmetric key, and using the shared symmetric key for
subsequent communication.

Using hybrid cryptography, I can send you an encrypted email by:

1. Generating a disposable symmetric key.
1. Using your public key to encrypt the symmetric key.
1. Using the symmetric key to encrypt my message.
1. Then I send you both ciphertexts.
1. You decrypt the symmetric key, and use it to decrypt the actual message.

Hybrid ciphers are widely used: TLS, SSH, and GnuPG use them.

A key concept in cryptography is **Kerckhoffs's principle**: a cryptosystem must
be secure if _everything about it is known_ but the private key. Therefore a
good cryptosystem is one that does not rely on secrecy of the algorithm or
implementation.
