---
title: A Syncthing and SQLite Gotcha
summary: On the rename system call.
---

So, I have this little app, [Epoch][epoch], that I use to keep a journal. It's a
tiny Rust web app that runs as a systemd service and uses [SQLite][sqlite] as
the database. I use a desktop and a [laptop][laptop] regularly, and use
[Syncthing][st] to synchronize them, including Epoch's database. That way I can
use the app on both devices without needing a server to synchronize them, the
tradeoff being that I have to make sure the sync is finished before performing
any mutations.

But I had this bug. Say I edit today's entry on the laptop, come home, wait for
Syncthing to finish, then I'd open today's entry on the desktop, and the text
would be missing. It's not that the server is holding a lock on the file and
preventing the sync: opening the database with the `sqlite3` command line tool
shows the new text is there. If I restart the server, Epoch can read the new
text.

My mental model was:

> The [rusqlite] `Connection` object points to the database file. Syncthing
> swaps the file's contents from under it. Subsequent queries go to the new
> file.

Turns out there's a very important part of POSIX filesystem semantics I was
ignorant of. The standard way to replace a file safely (i.e. atomically) is the
[`rename`][rn] system call:

```c
int rename(const char *old, const char *new);
```

Which Syncthing uses. This I know. What I didn't know is: what happens if other
processes had open file descriptors pointing to `new`? Do they see the new
contents?  No: those processes can keep reading and writing to the old file
_object_, but the file is orphaned in that no path points to it. And once all
file descriptors are released, the file becomes inaccessible.

I'm used to thinking of filesystem operations in terms of "this syscall takes a
path and gives you a pointer to the file, which you mutate directly". Whereas
`rename` works at the level of directory entries: it atomically mutates the
mapping from pathnames to files but doesn't touch files at all.

[epoch]: https://github.com/eudoxia0/epoch
[laptop]: /article/linux-on-the-fujitsu-lifebook-u729
[rn]: https://pubs.opengroup.org/onlinepubs/9799919799/functions/rename.html
[rusqlite]: https://github.com/rusqlite/rusqlite
[sqlite]: https://sqlite.org/
[st]: https://syncthing.net/
