---
layout: note
title: Chess Movement
description: "A cheatsheet for beginner's on how each of the pieces 
              move in chess, and how movement works during special 
              moves such as castling and en passant."
author: Stephen Czekalski
stylesheets: ["/assets/css/chess.css"]
group: chess
---

## Basic Movement
### The Pawn
{% 
    chessboard 
    WMf4 WMb3 WMb4 BPe5
    WPa2 WPb2 WPc2 WPd4 WPe4 WPf3 WPg2 WPh2 
    WRa1 WNb1 WBc1 WQd1 WKe1 WBf1 WNg1 WRh1
%}

* The pawn can never move or attack backwards.
* If the pawn hasn't moved yet it can move two squares forward.
* If the pawn has moved it can only move one square forward.
* The pawn can *only* attack one square diagonally. For example, the black pawn on **e5** could capture the white pawn on **d4**, but not the pawn on **e4**.
* Relative Value = 1


### The Knight
{% 
    chessboard 
    WNf3 WMg5 WMe5 WMd4 WMd2 WMe1 WMg1 WMh2 WMh4
%}

* Moves in an "L" shape.
* The only piece which can jump over other pieces.
* Relative Value = 3

### The Bishop
{% 
    chessboard 
    WBf3 WMe4 WMe2 WMg4 WMg2 WMh1 WMd1 WMh5 
    WMd5 WMc6 WMb7 WMa8
%}

* Can move diagonally as many squares as it wants.
* Can't jump over other pieces.
* Relative Value = 3

### The Rook
{% 
    chessboard 
    WRf3 WMf2 WMf4 WMe3 WMg3 WMf1 WMf5 WMf6 
    WMf7 WMf8 WMh3 WMd3 WMc3 WMb3 WMa3
%}

* Can move horizontally or vertically as many squares as it wants.
* Can't jump over other pieces.
* Relative Value = 5

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
* If your king is under attack it is put into check, and must be moved or protected.
* You lose if the King is put into checkmate.

## Special Moves
### Castling

Castling is the only move in chess where two pieces can move at once.
The move involves the player's king and either one of his or her rooks.

To castle, the king and rook involve must not have been moved.
Additionally, the spaces between the king and the rook must be clear.

{%
    chessboard
    WKe1 WRh1 WRa1
%}

To castle "kingside" or "short" move the king to **g1** and the right-most rook to **f1**.

{%
    chessboard
    WKg1 WRf1 WRa1
%}

To castle "queenside" or "long" move the king to **c1** and the left-most rook to **d1**

{%
    chessboard
    WKc1 WRh1 WRd1
%}

### En passant

### Pawn promotion

When a pawn reaches the back most rank of its opponent, it is promoted to a better piece.
It is most common for a player to promote their pawn to a queen.

## Relative Values
<table class="standard-table">
    <tr>
        <td>Piece</td>
        <td>Relative Value</td>
    </tr>
    <tr>
        <td>Pawn</td>
        <td>1</td>
    </tr>
    <tr>
        <td>Knight</td>
        <td>3</td>
    </tr>
    <tr>
        <td>Bishop</td>
        <td>3</td>
    </tr>
    <tr>
        <td>Rook</td>
        <td>5</td>
    </tr>
    <tr>
        <td>Queen</td>
        <td>9</td>
    </tr>
</table>