---
title: Domain-Agnostic and Domain-Specific Tools
summary: Software that can do everything does any one thing poorly.
card: domain-agnostic-and-domain-specific-tools.webp
---

This post is, in a sense, a continuation to [_Unbundling Tools for Thought_](/article/unbundling-tools-for-thought). It's an argument for why you shouldn't try to use a single tool to do everything, aimed at people who have been spent too much time shoveling prose into a "second brain" and have little to show for it.

---

Software tools span a spectrum from domain-agnostic to domain-specific.

Domain-agnostic tools are things like Obsidian. They have a small, spartan data model that can be made to represent most things. Obsidian's data model is just folders, pages, and links. Pages have a title and a body, and the body is text, and text is the universal interface. They have a small number of general workflows: creating a page, editing a page, viewing backlinks, text search. You can use them as a journal, a recipe app, a todo list, etc.

Domain-specific tools have a richer and more structured data model. Consider a CRM: there are first-class objects to represent people, companies, employment relations; these have rich attributes, you can represent "this person worked for this company in this position for this span of time" natively within the data model. This structure allows you to have a large number of much more specific workflows, like "see everyone who worked with this person" or "find everyone who worked for this company in 2016". But you can't use them outside the domain: you can't use a CRM as a recipe app.

And here's the asymmetry: while the tools can be domain-agnostic or domain-specific, your use cases are _always_ specific. You are always doing some concrete thing. And for any one specific use case, a specific tool can deliver a better ontology and a better UX than a general tool.

Because when you implement a specific use case in a domain-agnostic tool, you are always building _on top of_ the tool's data model. If you use e.g. Obsidian (or any other note-taking app) as, e.g., a CRM, there's an abstract concept of people, companies, employment, etc., but these concepts don't have a first-class existence, everything is concretely implemented as pages and links. You have a page to represent a person, a page to represent a company, and you use a link from the former to the latter to represent the "employed by" relation, and the corresponding backlink represents the "employs" relation.

At the ontology level, your data looks like this:

![TODO](/assets/content/domain-agnostic-and-domain-specific-tools/a.svg)

But the concrete data model looks like this:

![TODO](/assets/content/domain-agnostic-and-domain-specific-tools/b.svg)

And all the domain-specific nuances are hidden in text, invisible to software automation.

Whereas in a domain-specific tool, you are building _inside_ the data model: there's a table that implements the concept of "a person", it has a fixed set of attributes. At every point in time, the database has a fixed schema: you know all the attributes a company object can have, you know all your entries are consistent and coherent. Instead of a generic notion of a bidirectional link, you have first-class objects that represent relations: e.g. an employment relation that links people to companies is represented by a table that points the person and their employer and has metadata (the person's role, the start and end date of their employment).

When it comes to workflows, using a domain-agnostic tool means you either have to do most things by hand or through plugins. Doing it by hand is straightforwardly less efficient. But plugins never feel right. Often the UX feels janky because plugins are built to a lower standard. But ultimately plugins mean you go from a coherent, unified vision to a cacophony of a hundred visions which are mutually suspicious and subtly out of alignment with one another.

The main benefit of using a domain-agnostic app is that everything lives in the same data silo: you can cross-link data from many different disjoint use cases, e.g. journal entries and project documents and a reference library. Unlike web links, a single unified object graph can avoid dangling links, because the app can enforce link integrity. But this linking is hardly ever useful: do you actually need to have a bidirectional link between your journal entries and your recipes? Is there a benefit to this? You know where the link leads to. And links create a maintenance burden by making the entire graph structure more rigid.

So why do people use domain-agnostic apps at all? Partly, because a lot of use-cases are too rare or ad-hoc to require specific software. If you have three entries in a spreadsheet of book reviews, it's not necessary to go looking for a piece of software to manage them. This calculation will change as AI lowers the cost of software development.

But part of the reason is ideological: a lot of people bemoan data silos and have this aspirational idea that computing would be so much better if everything was more deeply interlinked. If you let go of this monistic obsession that everything under the Sun should go in the one giant object graph, and instead let each piece of data live in its own silo, you can be more effective.
