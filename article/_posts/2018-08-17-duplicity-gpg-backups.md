---
title: Portable Backups with Duplicity and GnuPG
summary: Managing on-site Duplicity backups with a secure GPG key.
tags: [crypto]
---

Disk encryption is necessary. But for local backups on removable media, using
the OS-native encryption mechanism hurts portability: [LUKS][luks] works on
Linux but not BSD, [geli][geli] works on FreeBSD but not Linux.

If you want to share data across heterogeneous systems, the solution is simple:
userspace encryption. Your files are stored on a lowest-common-denominator
filesystem anyone can access, and encryption is done in userspace. Rather than
manage this manually, I use [Duplicity][duplicity], which provides encrypted
incremental backups, both to locally-mounted storage or remote servers. It can
be used on both Linux and [FreeBSD][bsddupl], hence 'portable'. This guide will
go through setting up Duplicity with GnuPG and using a filesystem as the backup
destination (in my case, a removable USB drive).

# Creating a Storage Key

I'll assume you have [GnuPG][gpg] installed. To create a key, run:

~~~bash
gpg --full-generate-key
~~~

You will be prompted for the key's properties:

~~~
Please select what kind of key you want:
   (1) RSA and RSA (default)
   (2) DSA and Elgamal
   (3) DSA (sign only)
   (4) RSA (sign only)
Your selection? 1
RSA keys may be between 1024 and 4096 bits long.
What keysize do you want? (3072) 4096
Requested keysize is 4096 bits
Please specify how long the key should be valid.
         0 = key does not expire
      <n>  = key expires in n days
      <n>w = key expires in n weeks
      <n>m = key expires in n months
      <n>y = key expires in n years
Key is valid for? (0) 5y
Key expires at mié 16 ago 2023 16:14:21 -03
Is this correct? (y/N) y
~~~

Here I've chosen a 4096-bit long RSA key that expires in five years. After this
you'll be prompted for user-identifying information (full name, email).

Once the key has been constructed, `gpg` will show you the key ID, for example:

~~~
gpg: key F9D153FB59E40F3B marked as ultimately trusted
~~~

# Backups

Once you have the key ID, you can start making backups.

My USB drive is mounted at `/media/fernando/BACKUP/`. To back up my `~wiki/`
directory, I run:

~~~bash
PASSPRASE="my passphrase" \
          duplicity --encrypt-key F9D153FB59E40F3B \
          wiki/ \
          file:///media/fernando/BACKUP/duplicity/wiki/
~~~

Restoring files is equally simple and requires only swapping the source and
destination:

~~~bash
PASSPRASE="your key passphrase" \
          duplicity file:///my/backup/filesystem/dir/ \
          restored_dir
~~~

You will be prompted for the key passphrase, and when it's done you'll have a
`restored_dir` directory with the contents of the directory you originally
encrypted. You don't have to pass the `--encrypt-key` argument here since the
key ID is stored in the encrypted files, so GnuPG knows which private key to use
for decryption.

[luks]: https://en.wikipedia.org/wiki/Linux_Unified_Key_Setup
[geli]: https://man.freebsd.org/cgi/man.cgi?geli(8)
[duplicity]: https://en.wikipedia.org/wiki/Duplicity_(software)
[bsddupl]: https://man.freebsd.org/cgi/man.cgi?query=duplicity&apropos=0&sektion=0&manpath=FreeBSD+7.1-RELEASE+and+Ports&format=html
[gpg]: https://gnupg.org/
