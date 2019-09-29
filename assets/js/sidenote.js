/** The maximum width of a sidenote. */
var MAX_WIDTH = 600;

/** The minimum width of a sidenote. */
var MIN_WIDTH = 340;

var PADDING = 40;

/** Get the size of the left gutter. */
function _getLeftGutterSize() {
  return $(".col-lg-6").offset().left;
}

/** Get the width of the right gutter. */
function _getRightGutterSize() {
  return $(window).width() - ($(".col-lg-6").offset().left + $(".col-lg-6").width());
}

/** Get the width of the center column. */
function _getCenterColSize() {
  return $(".col-lg-6").width();
}

/** Positions sidenote-left divs horizontally. */
function _positionLeftSidenotesHorizontally(sidenote) {
  let gutter = _getLeftGutterSize();
  if(gutter >= MAX_WIDTH ) {
    sidenote.css("left", -(MAX_WIDTH + PADDING));
    sidenote.css("width", MAX_WIDTH);
    sidenote.css("visibility", "visible");

  } else if(gutter <= MAX_WIDTH && gutter >= MIN_WIDTH ) {
    sidenote.css("width", gutter);
    sidenote.css("left", -(gutter + PADDING));
    sidenote.css("visibility", "visible");

  } else {
    sidenote.css("visibility", "hidden");

  }
}

/** Positions sidenote-right divs horizontally. */
function _positionRightSidenotesHorizontally(sidenote) {
  let gutter = _getRightGutterSize();
  if(gutter >= MAX_WIDTH) {
    sidenote.css("left", _getCenterColSize());
    sidenote.css("width", gutter - 40);
    sidenote.css("visibility", "visible");

  } else if(gutter <= MAX_WIDTH && gutter >= MIN_WIDTH) {
    sidenote.css("left", _getCenterColSize());
    sidenote.css("width", gutter - 40);
    sidenote.css("visibility", "visible");

  } else {
    sidenote.css("visibility", "hidden");
    
  }
}

function _positionLeftSidenotesVertically(previous, sidenote) {
  if(previous == null)
  {
    let toc = $(".toc-wrapper");
    if(_doDivsOverlap(toc, sidenote)) {
      console.log(_getBottomOffset(toc) - sidenote.height());
      sidenote.css("top", _getBottomOffset(toc) - sidenote.height());
    }
  } else {
    sidenote.css("top", _getBottomOffset(previous) - sidenote.height());
  }
}

function _positionRightSidenotesVertically(previous, sidenote) {
}

function _doDivsOverlap(div1, div2) {

  if(div2.offset().top <= _getBottomOffset(div1))
  {
    return true;

  } else {
    return false;
  
  }

  return false;
}

function _getBottomOffset(div) {
  return div.offset().top + div.height(); 
}

function _positionLeftSidenotes() {
  let previous = null;
  $(".sidenote-left").each(function (index) {
    _positionLeftSidenotesHorizontally($(this))
    _positionLeftSidenotesVertically(previous, $(this));
    previous = $(this); 
  });
}

function _positionRightSidenotes() {
  let previous = null;
  $(".sidenote-right").each(function (index) {
    _positionRightSidenotesHorizontally($(this));
    _positionRightSidenotesVertically(previous, $(this));
    previous = null
  });
}

function _positionSidenotes() {
  _positionLeftSidenotes();
  _positionRightSidenotes();
}

$( document ).ready(function() {
  _positionSidenotes();
});

$(window).on('resize', function(){
  _positionSidenotes();
});