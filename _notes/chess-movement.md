---
layout: note
title: Chess Movement
description: "The basics of chess created to help teach new players."
author: Stephen Czekalski
stylesheets: ["/assets/css/chess.css"]
group: chess
---

## Basic Movement
### The Pawn
{% 
    chessboard 
    WMf4 WMb3 WMb4
    WPa2 WPb2 WPc2 WPd2 WPe2 WPf3 WPg2 WPh2 
    WRa1 WNb1 WBc1 WQd1 WKe1 WBf1 WNg1 WRh1
%}

* The pawn can never move or attack backwards.
* If the pawn hasn't moved yet it can move two squares forward.
* If the pawn has moved it can only move one square forward.
* The pawn can *only* attack one square diagonally.


### The Knight
{% 
    chessboard 
    WNf3 WMg5 WMe5 WMd4 WMd2 WMe1 WMg1 WMh2 WMh4
%}

* Moves in an "L" shape.
* The only piece which can jump over other pieces.

### The Bishop
{% 
    chessboard 
    WBf3 WMe4 WMe2 WMg4 WMg2 WMh1 WMd1 WMh5 
    WMd5 WMc6 WMb7 WMa8
%}

* Can move diagonally as many squares as it wants.
* Can't jump over other pieces.

### The Rook
{% 
    chessboard 
    WRf3 WMf2 WMf4 WMe3 WMg3 WMf1 WMf5 WMf6 
    WMf7 WMf8 WMh3 WMd3 WMc3 WMb3 WMa3
%}

* Can move horizontally or vertically as many squares as it wants.
* Can't jump over other pieces.
* In most cases, it is the second most powerful piece on the board.

### The Queen
{% 
    chessboard 
    WQf3 WMf2 WMf4 WMe4 WMe3 WMe2 WMg4 WMg3 WMg2
    WMh1 WMf1 WMd1 WMh5 WMf5 WMf6 WMf7 WMf8 WMh3 
    WMd3 WMc3 WMb3 WMa3 WMd5 WMc6 WMb7 WMa8
%}

* Can move diagonally, horizontally, or vertically as many squares as it wants.
* Can't jump over other pieces.
* The most powerful piece on the board.

### The King

{% 
    chessboard 
    WKf3 WMf2 WMf4 WMe4 WMe3 WMe2 WMg4 WMg3 WMg2
%}

* Can move one square diagonally in any direction.
* Can't jump over other pieces.
* Must be moved in check.
* You lose if the King is put into checkmate.

## Special Moves
### Castling

Castling is a special move which let's the player move two pieces at once.

### En passant

### Pawn promotion

When a pawn reaches the back most rank of its opponent, it can promote the pawn to a better piece.
It is most common for a player to promote their pawn to a queen.