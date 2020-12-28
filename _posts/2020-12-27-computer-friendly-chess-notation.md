---
layout: post
title: Computer Friendly Chess Notation
author: Stephen Czekalski
categories: [chess, programming]
---

A few weeks ago I watched *The Queen's Gambit* on Netflix.
After watching the first two episodes I decided to try my hand at chess.
I'm not great, but I did have fun.
Since then, I've been playing regularly against other people on [lichess.org](https://www.lichess.org/).
I've read, and skimmed, some books on basic tactics, watched some of [John Bartholomew's](https://www.youtube.com/channel/UC6hOVYvNn79Sl1Fc1vx2mYA) fantastic videos, and have generally tried to improve my play.

I saw a post on reddit, supposedly written by a GM, or maybe it was an IM, which said that one of the best ways to improve at chess is to write analyses for you games.
A standard rule of playing chess is for each player to record their moves.
Lichess.org, and all other chess software I've used, makes this easy by recording your moves automatically.

Typically, chess moves, as well as information about a given game, are stored in a plaintext format called [Portable Game Notation](http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm).
Here is the PGN for a game I played on lichess.org:


```plaintext
[Event "Rated Classical game"]
[Site "https://lichess.org/c0X6X15E"]
[Date "2020.12.22"]
[White "Redman_Reaper"]
[Black "stephencz"]
[Result "0-1"]
[UTCDate "2020.12.22"]
[UTCTime "19:53:05"]
[WhiteElo "977"]
[BlackElo "866"]
[WhiteRatingDiff "-41"]
[BlackRatingDiff "+26"]
[Variant "Standard"]
[TimeControl "1800+0"]
[ECO "C20"]
[Opening "King's Pawn Game: Leonardis Variation"]
[Termination "Normal"]

1. e4 e5 2. d3 Bb4+ 3. Nc3 a6 4. Nf3 Nf6 
5. Nxe5 Qe7 6. Bg5 Qxe5 7. Be3 d6 
8. Bd2 Nc6 9. b3 Bxc3 10. Bxc3 Qxc3+ 
11. Ke2 Bg4+ 12. f3 Bh5 13. h3 Nd4+ 
14. Kf2 Nxc2 15. Qxc2 Qxc2+ 16. Kg1 Qc3
17. Rc1 Qxc1 18. d4 O-O 19. a3 Qxa3 
20. h4 Qxb3 21. e5 Nd5 22. exd6 cxd6 
23. f4 Nxf4 24. Rh3 Nxh3+ 25. gxh3 Qg3+ 
26. Kh1 Bf3+ 27. Bg2 Bxg2+ 
28. Kg1 Rae8 29. h5 Re1# 0-1
```

A PGN file starts with headers, or what the standard calls "tag pairs".
As far as I can tell, these headers are completely optional, however the standard does define seven which are mandatory for "archival storage of PGN data".
In this particular PGN file, we can see who was playing, their elos, the outcome, the event, date, and other useful information.

After the headers, you have the movetext.
Movetext is recorded in [Standard Algebraic Notation](https://en.wikipedia.org/wiki/Algebraic_notation_(chess)).
A chessboard is an eight by eight grid of squares. 
The horizontal squares are labeled **a** through **h**.
And the vertical squares are labeled **1** through **8**.
Using this grid, Algebraic Notation lets us easily describe the movement and behavior of pieces in a game of chess.
For example:

```
1. e4 e5
2. Nc3 Nc6
3. Bb5 a6
4. Bxa6 Rxa6
```

This notation tells us that White opened the game by moving a pawn to **e4**, and Black followed up by moving their pawn to **e5**.
Then, to defend his pawn and therefore strength his hold on a center square, White responded to **Nc3** (their knight to **c3**).
Black reponsded in like with **Nc6**.
White, to directly threaten Black's knight on **c6**, moved **Bb5** (bishop to **b5**).
In an effort to push White's bishop on **b5** back, Black moved his pawn on **a7** to **a6**.
Unfortunately, White, being a new player, attacked Black's pawn on **a6**, with **Bxa6** (bishop captures **a6**).
And finally, Black captures White's bishop with **Rxa6** (rook to **a6**).

If you've never read Algebraic Notation before, or have never played the game of chess, than all of that might have been a bit confusing.
However, having played chess for only a few weeks at the time of writing this, I already feel comfortable with this form of notation.
Its easy to understand and interpret with a bit of practice. 

{:.drop-cap .}
In an effort to improve at chess, I decided to follow the advice I saw on reddit.
I decided that I would dedicate a part of this website's to analyzing my chess matches.
However, as amazing as the human mind is, picturing entire chess matches described only in Algebraic Notation is not something I can do.
It isn't something most people can do.
I needed visual support.
And for the sake of any possible readers, I wanted to provide visual support.

I resolved to write a plugin for my website that would be able to read PGN files, and display what the chessboard looked like at different points of a game.
As it turns out, this isn't so easy.

The problem with Portable Game Notation, is that the Algebraic Notation it uses to represent the movetext is what I would call a "human friendly format".
The human brain comes loaded with algorithms for disambiguating, interpreting, and visualizing information.
If you know the rules of chess.
And you know how to read Algebraic Notation.
Interpreting the meaning of movetext is trivial.
It is so trivial, that player's are expected to write it as they play chess.

Computer's are stupid.
Much more stupid than human beings.
This is a fact any programmer will tell you.
They can't do anything, without being told how to do it.

A program that tells a computer how to display PGN movetext has three major parts:

1. The [parser](https://en.wikipedia.org/wiki/Parsing) for converting the PGN file, both its headers and movetext, into a data structure which can be used more easily.
2. The [intepreter](https://en.wikipedia.org/wiki/Interpreter_(computing)) for simulating the moves that took place during the game on a chessboard. 
3. The renderer for drawing the final chessboard on the screen.

Regular expressions made writing the parser easy.
In less than an hour I had a parser that was able to store PGN headers and movetext in a [Hash](https://ruby-doc.org/core-2.7.2/Hash.html).
I was able to retrieve header information at will, and iterate over movetext move by move.
Likewise, the renderer wasn't a problem.
A chessboard is an eight by eight grid, so a function which generated an HTML table with some special CSS was all I needed.

The problem came from the intepreter.


