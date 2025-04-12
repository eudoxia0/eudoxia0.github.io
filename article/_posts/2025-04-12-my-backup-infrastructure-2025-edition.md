---
title: My Backup Infrastructure, 2025 Edition
summary: How I back up my personal data.
card: my-backup-infrastructure-2025-edition.webp
---

tl;dr two portable SSDs, synced with [rsync]; and a [Backblaze][bb] bucket synced with [restic].

I'm finally satisfied with my infrastructure for backups, so I'm writing it up so others can benefit from it.

![TODO TODO TODO](/assets/content/my-backup-infrastructure-2025-edition/infra.svg)

# Criteria

My requirements for backup infrastructure are:

1. Open source, to minimize the risk of backdoors.
1. Fast, but only incrementally: an initial snapshot can be slow.
1. Simple configuration, with little surface area to mess things up.
1. Encryption with keys that I control and which never leave my device. Ideally, encryption should be mandatory, to prevent accidentally putting cleartext on backup media.
1. Has to satisfy the 3-2-1 rule: at least three disjoint copies, in two different media, at leasto one off-site.
1. There has to be a known (documented, memorable) path to recovery. It would be embarrasing if you went to restore your backups and suddenly realized there's a missing link that prevents you from e.g. recovering the encryption key.

The one non-criterion is portability. Because I only use macOS, I don't need a solution where I can restore the backups from different operating systems.

# Local Backups

I have two portable SSDs, Chiron and Nessus, with encrypted [APFS]. The filesystem itself being encrypted is extremely convenient: I just plug them in, and the [macOS keychain][kc] has the keys. There's no possibility of accidentally leaking cleartext into the disk because the encryption is transparent.

I use rsync to synchronize the laptop to the disks. The specific incantation is:

```bash
rsync --progress --archive --human-readable \
      --executability --checksum --delete \
      ~/Root/ /Volumes/Chiron/Root
```

Which recursively copies the contents of `~/Root` into `/Volumes/Chiron/Root`, preserving permissions/times/the executable flag, using checksums rather than heuristics to see which files have changed, and deleting files that exist in the target but not the source.

Note that in rsync, trailing slashes matter! `rsync -a source target` creates a `source` directory inside `target`, while `rsync -a source/ target` syncs the contents of `source` inside `target`.

Why two disks? No reason. [Why have one when you can have two for twice the price?][hadden]

# Remote Backups

Continuing with the centaur naming convention, I have a Backblaze bucket named Pholus, and I use restic to take snapshots of the laptop and upload them to the bucket.

Why Backblaze? Because it's cheaper than [S3], and less involved than S3 (no IAM/roles/policies/etc.) and it does one thing and does it well.

Why restic? Because everything else is worse. Duplicity requires using GnuPG for key management, which is like if to start a car you had to stab yourself with your keys. borg is written in Python, which is usually a bad sign for performance and user experience. rclone, by default, is just cloud rsync, it doesn't encrypt anything, you have to use a two-level configuration, where a `crypt` backend acts as a proxy to the real storage backend. So if you misconfigure things, you could end up writing cleartext to the cloud.

restic is easy to learn. The ontology is: you have a thing called a repository, which could be a local directory or a remote object store, identified by a path and locked with a password. A repository has a list of snapshots, which are like Git commits: a snapshot of a directory at a point in time. Backing up creates a new snapshot, you can list the snapshots, restore a specific one, `ls` into them, and download a single file out of a snapshot.

I recommend trying out the commands using local repositories, where the data is stored in a directory. That lets you get a hang of the ontology and the commands. Then you can create a repository backed by cloud storage.

restic supports Backblaze directly, but the documentation recommends using Backblaze's S3-compatible API. To do that, when creating a key, you have to tick a particular box. Then you have to know how to map the key properties to the AWS environment variables. That is the only difficulty.

# Frequency

I have a recurring task on my todo list whereby, once a week, I plug in the external drives, run the backup script, and also take a restic snapshot.

[rsync]: https://en.wikipedia.org/wiki/Rsync
[restic]: https://restic.net/
[bb]: https://www.backblaze.com/
[S3]: https://aws.amazon.com/s3/
[APFS]: https://en.wikipedia.org/wiki/Apple_File_System
[kc]: https://en.wikipedia.org/wiki/Keychain_(software)
[hadden]: https://www.youtube.com/watch?v=Et4sMJP9FmM
