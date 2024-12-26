---
title: How I Use Claude
summary: Exploring Anthropic's data export.
---

I was a casual user of [Claude] until the [23 October release][rel] ([informally][inf] "Claude 3.6"), when it crossed a quality threshold I didn't even know was there. It is really, really good. I have been using it a lot more since, and I got curious as to how much more.

You can [export][ex] your Claude conversation history. It's just a zip file with a massive JSON blob. I wrote the types to represent it, and Claude wrote most of the [matplotlib]. The code is [here][repo], and it's set up so you can run it on your own export very easily.

# Quantitative Change

The most striking change is the cumulative amount of words I have written to Claude. You don't need any statistics to see hard takeoff:

![A plot of the cumulative amount of words I've written to Claude over time. The x-axis is time, discretized into weeks. The x-axis begins on April 2024 and ends in December 2024. The y-axis is the word count, ranging from 0 to 70,000. The curve is flat at the start, shows slow (and slowing) growth in the middle of the year, and goes up much faster after late October 2024. A red vertical line indicates the release date of Claude 3.6.](/assets/content/how-i-use-claude/cw.png)

On a weekly basis; conversations, messages, and words written to Claude show a visible discontinuity:

![A plot of conversations per week. Same x-axis as above. The y-axis ranges from 0 to 60. There's a bump around April with a peak at ~45 conversations, then a gradual slowdown. In the middle of the year, the numbers sit at around ten conversations a week. After the release of Claude 3.6, the number of conversations grows markedly.](/assets/content/how-i-use-claude/cpw.png)

![A plot of messages sent to Claude per week. Same x-axis as above. The y-axis ranges from 0 to 270. The numbers are under 50 messages for most of the year. After the release of Claude 3.6, the numbers increase sharply, and are rarely below 100.](/assets/content/how-i-use-claude/mpw.png)

![A plot of words written to Claude per week. Same x-axis as above. The y-axis ranges from 0 to 10000. The numbers are under 2000 words per week for most of the year. After the release of Claude 3.6, the numbers increase sharply. In December, they are never under 4000 words per week.](/assets/content/how-i-use-claude/wpw.png)

And these are the daily averages:

|               | Before 3.6 | After 3.6 | Change     |
|---------------|-----------:|----------:|-----------:|
| Conversations | 1.6        | 5.6       | 252%       |
| Messages      | 3.7        | 20.2      | 449%       |
| Words         | 89.5       | 740.6     | **727%**   |

Finally, I took the top 30 conversations by message count and manually classified them:

![A pie chart of categories with the following values: Advice 37%, Exploration 33%, Code 13%, Writing 10%, Lifting 7%.](/assets/content/how-i-use-claude/categories.png)

With the categories being:

- "Advice" is me trying to work something out: overcome anxiety, aversion, decision paralysis. Claude is really good at helping here, mostly because thinking quickly saturates: when you've thought about a problem for five minutes, you've had all the thoughts you're gonna have, and it's time to talk to someone else. Claude let's me sample fresh perspectives and possible actions I had not thought of.
- "Exploration" is exploring ideas together, including both vague open-ended conversations, and more concretely trying to learn something.
- "Code" is roughly 50% asking Claude to write something for me, and 50% using Claude as a rubber ducky: talking possibilities, tradeoffs, etc.
- "Writing" is asking Claude to critique something I wrote.
- Finally, "lifting" is lifting-specific questions.

# Qualitative Change

I remember, halfway through the year, feeling guilty for paying $20 a month for a $2000/hr consultant on every topic who is infinitely industrious and instantly available, and then hardly using it. If phones are the library of Alexandria in our pocket, Claude is like having [Aristachos of Samos][ari] on retainer.

What changed after 3.6, that before I had to actively try to find uses for Claude, and now I'm mired in more possibilities than I have time to pursue?

There's a behaviourist explanation: every time Claude excels at something, the "use Claude" heuritic gets bumped up, leading to a virtuous cycle of using Claude in more and more adjacent tasks. And so the next use of Claude suggests itself.

There's a performance explanation: Claude 3.6 is much more reliable than 3.5. Before, when using LLMs as tutors, I was always on guard for hallucinations. And the constant skepticism is a big source of friction. The first thing I noticed after the 3.6 release is that hallucination is basically solved for major topics (though Claude will make mistakes for e.g. the minutiae of some Python library). So if you want to learn some well-trod subject, like French or special relativity, Claude is great.

For this reason I often use Claude where it is not a natural fit, like for web search. Googling something feels like wading through an ocean of sewage. Asking Claude is always at least okay, and often pleasant. So I don't mind that I'm asking a compressed, nine month old snapshot of the web. Had I the time I'd [give][tool] Claude access to [Kagi] or [DDG], and never use Google again.

But then there's the affective explanation: talking to Claude feels like using the [Primer][da]. And though there is no Miranda on the other side, it is no less magical.

Claude is genuinely engaging. Exploring ideas with Claude is _fun_. You can have genuinely interesting, intellectual conversations with Claude. It knows everything, so you can use highly idiosyncratic language, or draw analogies from anywhere, and Claude will keep pace.

And it is charming. Sometimes, at the end of a long conversation, I will ask it to generate whatever it wants. It is delightful to see how it expresses itself: sometimes it makes these little React [toys], sometimes it tries to make art with SVGs. And the SVGs rarely look like anything. But it does its best. Here, for example, is Claude's depiction of paradise:

![Claude's stylized illustration of Paradise. According to the artist, the elements include: a stone fountain, fruit trees, stylized animals, birds in flight, flowers, a backdrop of mountains, an ornamental frame.](/assets/content/how-i-use-claude/paradise.webp)

I feel privileged to live in these times. In a wholly undeserved sense, talking to Claude makes you feel proud to be human: we can build miraculous things.

[Claude]: https://claude.ai
[rel]: https://www.anthropic.com/news/3-5-models-and-computer-use
[inf]: https://x.com/search?q=%22claude+3.6%22
[ex]: https://support.anthropic.com/en/articles/9450526-how-can-i-export-my-claude-ai-data
[matplotlib]: https://matplotlib.org/
[ari]: https://en.wikipedia.org/wiki/Aristarchus_of_Samos
[da]: https://en.wikipedia.org/wiki/The_Diamond_Age
[repo]: https://github.com/eudoxia0/claude-export
[Kagi]: https://kagi.com/
[DDG]: https://duckduckgo.com/
[tool]: https://docs.anthropic.com/en/docs/build-with-claude/tool-use
[toys]: https://x.com/zetalyrae/status/1855813597131096547
