---
layout: note
title: Test Analysis
author: Stephen Czekalski
description: A collection of analyses I've gone for some of my chess games in an effort to improve.
group: chess-analysis
stylesheets: ['https://unpkg.com/@chrisoakman/chessboardjs@1.0.0/dist/chessboard-1.0.0.min.css']
scripts: ['https://unpkg.com/@chrisoakman/chessboardjs@1.0.0/dist/chessboard-1.0.0.min.js']
---

<div id="board" style="width: 400px"></div>

<script>
var board = Chessboard('board', {
  draggable: true,
  dropOffBoard: 'trash',
  sparePieces: true
})

$(document).ready(board.start)

</script>