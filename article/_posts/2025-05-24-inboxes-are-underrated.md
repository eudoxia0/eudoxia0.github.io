---
title: Inboxes are Underrated
summary: On inboxes as application-specific todo lists.
card: inboxes-are-underrated.webp
---

I have a lot of communication apps. By volume: Twitter DMs, Signal, Whatsapp, iMessage, Discord, email. Because I have so many disjoint places where communication happens, I have a daily task on [Todoist] to go through each of these, and ensure that every conversation is handled, where "handled" means: if I can reply immediately, I do so; otherwise, I make a task to reply. Polling is better than interrupts.

But this is imperfect, because often I get distracted, and I do neither. Sometimes I read the other person's message, and mentally begin drafting a reply, but forget to make a task. Sometimes I check DMs outside of this timeblock, when I'm less disciplined about following the checklist. Sometimes I'm interrupted before I can create the task. And so on. And all of these systems have a concept a conversation being read/unread, but it is fragile: touch it and it goes away. So if I don't reply immediately, and I don't make a task, I might never reply. And then new conversations pile up, burying the old ones.

Email is where I get the least human communication, but it is the one system that has an inbox. And the inbox is invaluable for me, because it acts as a domain-specific todo list: it draws a hard line between the things that have been handled (archived), and the things that are not (inbox). Crossing this line requires an _explicit_ act.

With email, I can execute this algorithm:

- For each conversation in the inbox:
  - If it's spam, delete it.
  - If it doesn't need a reply, archive it.
  - If I can reply immediately, reply and archive the conversation[^gtd].
  - If I can't reply immediately, make a task to reply.

Because archiving requires an explicit action, there's no possibility of forgetting to handle a conversation.

This is the utility of inbox zero: it has no false negatives! If the inbox is empty, I _know_ that all of my correspondence has been handled. If the inbox is non-empty, I know there is work to do.

Why do so few apps have inboxes? Probably because most people never archive their emails, they just keep everything in the inbox. And probably the concept of an inbox reminds them of email, and email feels old and corporate and spammy. Most of the email I get is transactional (e.g. login codes), notifications, and spam.

For people like me who want to be conscientious about communication, and who need mechanical help to achieve that, the lack of an inbox is really, really frustrating.

And while inboxes could be entirely local to the client software, the protocol doesn't have to implement the inbox/archive distinction. But communication protocols are increasingly [locked down][cf], so that you can't bring your own client, with your own features.

Tangentially: inbox zero is not an obvious practice at all. Rather than relying on the user to implement the inbox zero workflow, the software should make triaging a first-class workflow. Like spaced repetition: you open [Anki], click "Study", go through the flashcards due today, choosing either "Forgot" or "Remembered". You open the email client, click "Triage", and go through one conversation at a time, and choose either "Delete", "Archive", "Reply", or "Skip".

# Footnotes

[^gtd]: Usually I archive a conversation immediately after replying, but sometimes you need a reply from the other person. So I make a task on my todo list that says "Waiting for a reply from X". The idea is from [_Getting Things Done_][gtd]. If the person doesn't reply, the existence of the task reminds me to ping them again. Otherwise I will certainly forget about it.

[Anki]: https://apps.ankiweb.net/
[Todoist]: https://www.todoist.com/
[cf]: /article/client-freedom
[gtd]: https://en.wikipedia.org/wiki/Getting_Things_Done
