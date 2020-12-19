---
layout: note
title: Chess
description: "Notes on the game of Chess ranging from the basics of play to the complexity of tactics."
author: Stephen Czekalski
stylesheets: ["/assets/css/chess.css"]
group: games
---

## Basics

Chess is a two player board game which has been played for over a century.
The objective of the game is to capture the other player's king by positioning your own pieces strategically.
The game ends when one player captures the other's king, there is a stalemate, or if one player resigns.

### Objective and Setup

The objective of the game is to capture the other player's king.
To do this the player must weaken their opponent by capturing their other pieces and outwitting them.

Within the game of chess there are six types of pieces: [The King](#the-king), [Queen](#the-queen), [Rook](#the-rook),
 [Bishop](#the-bishop), [Knight](#the-knight), and [Pawn](#the-pawn).
Each of the types of pieces move in a special way.
At the start of a game of chess each player should have the following pieces:

* King x 1
* Queen x 1
* Rook x 2
* Bishop x 2
* Knight x 2
* Pawn x 8

The board at the start of a new game should look like this:

{% 
    chessboard 
    BPa7 BPb7 BPc7 BPd7 BPe7 BPf7 BPg7 BPh7 
    BRa8 BNb8 BBc8 BQd8 BKe8 BBf8 BNg8 BRh8
    WPa2 WPb2 WPc2 WPd2 WPe2 WPf2 WPg2 WPh2 
    WRa1 WNb1 WBc1 WQd1 WKe1 WBf1 WNg1 WRh1
%}

Each side of the board is a mirror image.
Pawns occupy the second rank (**a2-h2**).
Rooks go in the corners at **a1** and **h1**.
The Knights go next to the Rooks.
The Bishops go next to the Knights. 
The Queen is positioned at **d1**.
Finally, the King is placed **e1**.

The player with the light colored pieces is called "**White**".
And the player with the dark colored pieces is called "**Black**".
It is standard for White to go first, followed by Black.

### The King

The King is your most important piece. 
If you lose it, you lose the game.
Whenever a King is under attack it is said to be in "[check](#check-and-checkmate)".
When your King is in check you **must move it**.
If your King is in check, but there is no way to block the attack or move out of check, this is called "[checkmate](#check-and-checkmate)".
When your King is put into checkmate you lose the game.
If your King is *not* in check and you can make no legal move, the game ends in a "[stalemate](#stalemate)".

{% 
    chessboard 
    WKf3 WMf2 WMf4 WMe4 WMe3 WMe2 WMg4 WMg3 WMg2
%}

Despite the fact that the King is your most important piece, it's ability to move is limited when compared to the other pieces.
The King can only move square in any direction.

* Can move one square diagonally in any direction.
* Can't jump over other pieces.
* Must be moved in check.
* You lose if the King is put into checkmate.

### The Queen

The Queen is the most powerful pieces on the board. 
It has the combined powers of a [Rook](#rook) and a [Bishop](#bishop).
The Queen can move diagonally, horizontally, or vertically as many squares as it wants.
The Queen **cannot jump over** other pieces.
If a piece is in its path it must either capture or move it.

{% 
    chessboard 
    WQf3 WMf2 WMf4 WMe4 WMe3 WMe2 WMg4 WMg3 WMg2
    WMh1 WMf1 WMd1 WMh5 WMf5 WMf6 WMf7 WMf8 WMh3 
    WMd3 WMc3 WMb3 WMa3 WMd5 WMc6 WMb7 WMa8
%}

Being the most powerful piece on the board, it is a piece you should be exceptionally careful with.
Losing this piece can mean the different between victory and defeat.

* Can move diagonally, horizontally, or vertically as many squares as it wants.
* Can't jump over other pieces.

### The Rook

The Rook can move horizontally or vertically as many squares as it wants.
Like the Queen it **cannot jump over** other pieces.
If a piece is in its path it must either capture or move it.

{% 
    chessboard 
    WRf3 WMf2 WMf4 WMe3 WMg3 WMf1 WMf5 WMf6 
    WMf7 WMf8 WMh3 WMd3 WMc3 WMb3 WMa3
%}

* Can move horizontally or vertically as many squares as it wants.
* Can't jump over other pieces.

### The Bishop

The Bishop can move diagonally as many squares as it wants.
Like the Queen it **cannot jump over** other pieces.
If a piece is in its path it must either capture or move it.

{% 
    chessboard 
    WBf3 WMe4 WMe2 WMg4 WMg2 WMh1 WMd1 WMh5 
    WMd5 WMc6 WMb7 WMa8
%}

* Can move diagonally as many pieces as it wants.
* Can't jump over other pieces.

### The Knight

The Knight moves in a unique "L" shape.
The Knight is unique in that is **can jump over** other pieces.
It is the only piece in the game of chess which can jump over other pieces. 

{% 
    chessboard 
    WNf3 WMg5 WMe5 WMd4 WMd2 WMe1 WMg1 WMh2 WMh4
%}

* Moves in an "L" shape.
* The only piece which can jump over other pieces.

### The Pawn

The Pawn is the weakest and least value piece on the board.
The rules governing the movement of a pawn are slightly more complicated than the other pieces.
Firstly, the pawn can only move forward. A pawn **never** moves backwards.

If a pawn has yet to be moved from its starting position, its can move two spaces forward.
For example, see how the pawn at **b2** can move to either **b3** or **b4**.
After a pawn moves from its starting position it can only move one space forward.
For example, notice how the pawn at **f3** can only move to **f4**.

{% 
    chessboard 
    WMf4 WMb3 WMb4
    WPa2 WPb2 WPc2 WPd2 WPe2 WPf3 WPg2 WPh2 
    WRa1 WNb1 WBc1 WQd1 WKe1 WBf1 WNg1 WRh1
%}

To make matters more complicated, the pawn does not attack like the other pieces.
While the pawn can only move forward, it can only attack diagonally.

{%
    chessboard
    WPe4 BQd5 BPf5 BNe5
%}

In the above diagram which pieces can White's pawn capture?
White's pawn can capture either the queen at **d5** or Black's pawn at **f5**.
Because pawns can only attack diagonally, it **cannot** capture Black's knight at **e5**.

* Can move one or two squares forward if it hasn't been previously moved.
* Once moved, it can only move one square forward.
* It can only attack one square diagonally.
* It can not move back.

### *En Passant*

*En Passant* is a special type of pawn capture which can only occur in a specific situation.
Consider the following board:

{% 
    chessboard 
    BPa7 BPb7 BPc7 BPd7 BPe7 BPf7 BPg7 BPh7 
    BRa8 BNb8 BBc8 BQd8 BKe8 BBf8 BNg8 BRh8
    WPa2 WPb2 WPc2 WPd2 WPe5 WPf2 WPg2 WPh2 
    WRa1 WNb1 WBc1 WQd1 WKe1 WBf1 WNg1 WRh1
%}

White has a pawn at **e5**. 
If Black were to move his pawn either at **d7** or **f7** to **d5** or **f5**, then White
could capture Black's pawn using *En Passant*.

{% 
    chessboard 
    BPa7 BPb7 BPc7 BPd5 BPe7 BPf7 BPg7 BPh7 
    BRa8 BNb8 BBc8 BQd8 BKe8 BBf8 BNg8 BRh8
    WPa2 WPb2 WPc2 WPd2 WPe5 WPf2 WPg2 WPh2 
    WRa1 WNb1 WBc1 WQd1 WKe1 WBf1 WNg1 WRh1
    WMd6
%}

For example, here we can see Black moved his pawn to **d5**.
On White's turn he has a chance to capture Black's pawn like so.
If White does not capture Black's pawn **immediately**, he has lost
his chance to capture it with *En Passant*.

{% 
    chessboard 
    BPa7 BPb7 BPc7 BPe7 BPf7 BPg7 BPh7 
    BRa8 BNb8 BBc8 BQd8 BKe8 BBf8 BNg8 BRh8
    WPa2 WPb2 WPc2 WPd2 WPd6 WPf2 WPg2 WPh2 
    WRa1 WNb1 WBc1 WQd1 WKe1 WBf1 WNg1 WRh1
%}

Here is an example of the same situation but for Black.

{% 
    chessboard 
    BPa7 BPb7 BPc4 BPd7 BPe7 BPf7 BPg7 BPh7 
    BRa8 BNb8 BBc8 BQd8 BKe8 BBf8 BNg8 BRh8
    WPa2 WPb2 WPc2 WPd2 WPe2 WPf2 WPg2 WPh2 
    WRa1 WNb1 WBc1 WQd1 WKe1 WBf1 WNg1 WRh1
%}

{% 
    chessboard 
    BPa7 BPb7 BPc4 BPd7 BPe7 BPf7 BPg7 BPh7 
    BRa8 BNb8 BBc8 BQd8 BKe8 BBf8 BNg8 BRh8
    WPa2 WPb2 WPc2 WPd4 WPe2 WPf2 WPg2 WPh2 
    WRa1 WNb1 WBc1 WQd1 WKe1 WBf1 WNg1 WRh1
    BMd3
%}

{% 
    chessboard 
    BPa7 BPb7 BPd3 BPd7 BPe7 BPf7 BPg7 BPh7 
    BRa8 BNb8 BBc8 BQd8 BKe8 BBf8 BNg8 BRh8
    WPa2 WPb2 WPc2 WPe2 WPf2 WPg2 WPh2 
    WRa1 WNb1 WBc1 WQd1 WKe1 WBf1 WNg1 WRh1
%}

* *En Passant*

### Pawn Promotion


### Castling

### Check and Checkmate

### Stalemate

### Resignation

## Chess Notation