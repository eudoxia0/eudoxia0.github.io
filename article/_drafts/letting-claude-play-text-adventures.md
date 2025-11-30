---
title: Letting Claude Play Text Adventures
summary: Experiments in cognitive architecture.
---

Last week I went to an [AI hackathon][hack].

- cog arch
  - gofai inspired by cog sci
  - soar, act-r
  - never really done anything particuarly impressive
  - llm agents are "accidental cog archs"
    - have the same kind of modules: task lists, long term memory
  - can agent performance improve fi you build an agent arch based on a
    "principled" cog arch?
  - then i thought: what do i apply this to?
    - math?
    - chatbot?
    - code?
    - text adventures!
      - stylized world
      - long-term goal
      - puzzles
      - exploration
      - the text adventiure world model resembes a gofai world model
- text adventures
  - found dfrotz interpreter
  - i built a really simple harness
  - haiku wanders around
  - sonnet finishes the first task, then wanders around
  - anchorhead is not the ideal choice for a test enviroment
    - the game is too long
    - the game is very open-world, and from the very start you can wander around
      most of the game map before you are forced to engage with the first puzzle
      of the game
  - needed something smaller
  - claude can make its own games
    - there's no inform7 package for nixos
    - so i had to use inform6
    - asked claude to write small "Portal test chamber" type games
      - a small, single room, escape the room type game, which Claude beat in less than 10 turns
      - then larger, multi-room heist game
        - this one was more fun
        - claude beats it in the normal harness but it takes a while
        - i thought it would be large enough to try a different harness
        - new harness: show only the last five turns, give claude a working memory
        - claude gets caught up in all this bullshit
          - trapped by a red herring: a garden with a well, kept trying to go down the well

[hack]: https://luma.com/ycc02hpc
