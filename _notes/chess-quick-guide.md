---
layout: note
title: Chess Quick Guide
description: "The basics of chess created to help teach new players."
author: Stephen Czekalski
stylesheets: ["/assets/css/chess.css"]
group: chess
---

## Objective
The objective of chess is to capture the other player's [King](#the-king).
This is done by strategically weakening, capturing, and outmaneuvering your oppenent with your pieces.

## Setup

In chess each player gets sixteen pieces:

* Pawn x 8
* Knight x 2
* Bishop x 2
* Rook x 2
* Queen x 1
* King x 1

The following symbols are used to represent each of these pieces:

<table class="chess" style="width: 100%; text-align: left; border: none">
<tr>
    <th>Piece</th>
    <th>White</th>
    <th>Black</th>
</tr>
<tr>
    <td>Pawn</td>
    <td class="white-pawn"></td>
    <td class="black-pawn"></td>
</tr>
<tr>
    <td>Knight</td>
    <td class="white-knight"></td>
    <td class="black-knight"></td>
</tr>
<tr>
    <td>Bishop</td>
    <td class="white-bishop"></td>
    <td class="black-bishop"></td>
</tr>
<tr>
    <td>Rook</td>
    <td class="white-rook"></td>
    <td class="black-rook"></td>
</tr>
<tr>
    <td>Queen</td>
    <td class="white-queen"></td>
    <td class="black-queen"></td>
</tr>
<tr>
    <td>King</td>
    <td class="white-king"></td>
    <td class="black-king"></td>
</tr>
</table>

The board should be positioned so that the right corner of each player's side of the board is the white/lightest colored tile.
The pieces should be place like this:

{% 
    chessboard 
    BPa7 BPb7 BPc7 BPd7 BPe7 BPf7 BPg7 BPh7 
    BRa8 BNb8 BBc8 BQd8 BKe8 BBf8 BNg8 BRh8
    WPa2 WPb2 WPc2 WPd2 WPe2 WPf2 WPg2 WPh2 
    WRa1 WNb1 WBc1 WQd1 WKe1 WBf1 WNg1 WRh1
%}

The player with the lighest colored pieces is called **White**.
The player with the darkest colored pieces is called **Black**.
The player's take turns making one legal move per turn.
White *always* goes first.

## How the Game Ends
### Check and Checkmate
You *cannot* allow your king to be captured.
A king is said to be "in check" if it is directly under attack.
When your king is in check you *must* either **(a)** move your king so that it is no longer under attack, **(b)** block the attack with another piece, **(c)** capture the attacking piece.

When your king is directly under attack, but the attack cannot be evaded or blocked your king is "checkmated".
When this happens you lose the game.

### Stalemate
If your king is not in check, but you can make no valid moves, the game ends in a stalemate. 

### Resignation and Draw
If you feel your position is hopeless, you can resign to end the game before being checkmated.
Optionally, you can offer a draw in which both player's agree that neither wins or loses.

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
### En passant
### Pawn promotion

## Beginner Tactics

### Defend Your Pieces

A common mistake new player's make is moving pieces into positions where the are undefend.
When a piece is undefended it can be taken without consequence.
Obviously, there are exceptions to this rule, however it is generally considered a good idea to properly defend your pieces.
For example:

{% 
    chessboard 
    BPe6 BPg6 WNf4 BPd7
%}

In this situation Black's pawn on **e6** is defended by its pawn on **d7**.
However, the pawn on **g7** can be taken by White's knight without consequence.

{% 
    chessboard 
    BPe6 BPg6 WNf4 BPd7 BRg8
%}

In this next scenario, the pawn on **g6** is defended by Black's rook on **g8**.
The fact that both of these pawns are defended, makes it less likely for White to attack them.
If he does attack them then he will be paying for it with his knight.
