/** True when the module has been initialized. Otherwise false. */
var _INITIALIZED = false;

/** The minimum distance from the top of the page that a left sidenote can appear.*/
var _LEFT_MIN_TOP_OFFSET = undefined;

/** The minimum distance from the top of the page that a right sidenote can appear.*/
var _RIGHT_MIN_TOP_OFFSET = undefined;

/** The max width of a sidenote box. */
var _BOX_MAX_WIDTH = undefined;

/** The min width of a sidenote box before it disappears. */
var _BOX_MIN_WIDTH = undefined;

/** The center column that the sidenotes will be positions left and right of. */
var _CENTER_COL = undefined;

/**
 * Initializes the values used by sidenote.js to determine the sizing and
 * positioning of sidenotes
 * @param {*} leftOffset The number of pixels from the top that sidenotes can be made visible.
 * @param {*} rightOffset The number of pixels from the top that sidenotes can be made visible.
 * @param {*} boxMin The minimum width of sidenote boxes.
 * @param {*} boxMax The maximum width of sidenote boxes.
 * @param {*} central The class of the central column/div to position sidenotes to the left and right of.
 */
function init(leftOffset, rightOffset, boxMax, boxMin, central) {
  this._LEFT_MIN_TOP_OFFSET = leftOffset;
  this._RIGHT_MIN_TOP_OFFSET = rightOffset;
  this._BOX_MAX_WIDTH = boxMax;
  this._BOX_MIN_WIDTH = boxMin;
  this._CENTER_COL = central;
  this._INITIALIZED = true;
}

/**
 * Initializes sidenotes with the values used by sidenote.js to determine the sizing
 * and positioning of the sidenotes, and creates and configures the sidenotes.
 * @param {*} leftOffset The number of pixels from the top that sidenotes can be made visible.
 * @param {*} rightOffset The number of pixels from the top that sidenotes can be made visible.
 * @param {*} boxMin The minimum width of sidenote boxes.
 * @param {*} boxMax The maximum width of sidenote boxes.
 * @param {*} central The class of the central column/div to position sidenotes to the left and right of.
 */
function initSidenotes(leftOffset, rightOffset, boxMax, boxMin, central) {
  this.init(leftOffset, rightOffset, boxMax, boxMin, central);

  createLeftSideNotes();  
  createRightSideNotes();

  configureLeftSideNotes();
  configureRightSideNotes();

  $( window ).resize(function() {
    configureLeftSideNotes();
    configureRightSideNotes();
  });
}

/** Configures the size and position of the sidenotes in the left margin. */
async function configureLeftSideNotes() {
  var leftMargin = getLeftMarginWidth();
  var offsets = getLeftSideNoteOffsets();

  var boxWidth = getLeftBoxWidth();
  var boxPosition = leftMargin - boxWidth;

  var previousOffset = 0;
  var previousHeight = 0;

  $("div.sidenote-left-box").each(function(index) {

    //If the sidenote's top offset is past the min top offset distance,
    //and the left margin is greater than the minimum box width, then
    //we make is visible, and give it the proper width and positioning.
    if(offsets[index] >= _LEFT_MIN_TOP_OFFSET && leftMargin >= _BOX_MIN_WIDTH) {
      $(this).css("visibility", "visible");
      $(this).css("width", boxWidth);
      $(this).css("left", boxPosition);

      //If the top offset of the current sidenote
      if(index > 0 && offsets[index] <= previousOffset + previousHeight) {
        offsets[index] = offsets[index - 1] + previousHeight;
        $(this).css("top", offsets[index]);

      } else {
        $(this).css("top", offsets[index]);
      }
      
      //The offset and height of the previous sidenote.
      previousOffset = offsets[index];
      previousHeight = $(this).height();

    } else {
      //If the sidenote is either too high on the page or the
      //margin is too  narrow, hide it.
      $(this).css("visibility", "hidden");
    }
  });
}

/** Configures the size and position of the sidenotes in the right margin. */
async function configureRightSideNotes() {
  var rightMargin = getRightMarginWidth();
  var offsets = getRightSideNoteOffsets();

  var boxWidth = getRightBoxWidth();
  var boxPosition = rightMargin - boxWidth;

  var previousOffset = 0;
  var previousHeight = 0;

  $("div.sidenote-right-box").each(function(index) {

    //If the sidenote's top offset is past the min top offset distance,
    //and the right margin is greater than the minimum box width, then
    //we make is visible, and give it the proper width and positioning.
    if(offsets[index] >= _RIGHT_MIN_TOP_OFFSET && rightMargin >= _BOX_MIN_WIDTH) {
      $(this).css("visibility", "visible");
      $(this).css("width", boxWidth);
      $(this).css("right", boxPosition);

      //If the top offset of the current sidenote
      if(index > 0 && offsets[index] <= previousOffset + previousHeight) {
        offsets[index] = offsets[index - 1] + previousHeight;
        $(this).css("top", offsets[index]);

      } else {
        $(this).css("top", offsets[index]);

      }
      
      //The offset and height of the previous sidenote.

      previousOffset = offsets[index];
      previousHeight = $(this).height();

    } else {
      //If the sidenote is either too high on the page or the
      //margin is too  narrow, hide it.
      $(this).css("visibility", "hidden");
    }
  });
}

/** Create sidenotes in the left margin. */
function createLeftSideNotes() {
  createDivInBody("sidenote-left", "sidenote-left-box");
}

/** Create sidenotes in the right margin. */
function createRightSideNotes() {
  createDivInBody("sidenote-right", "sidenote-right-box");
}

/**
 * Create div in the body tag for every instance of a certain class.
 * @param {*} className The class to create a new div for every instance of.
 * @param {*} boxName The class/classes to apply to the new div.
 */
function createDivInBody(className, boxName) {
  $("div." + className).each(function(index) {
    var contents = $(this).clone(); //Copy contents of sidenote-marker
    $(this).empty(); // Clear out the div.
    $("body").append("<div class='"+ boxName + "'>" + contents.html() + "</div>"); //Create box div and insert contents.
  });
}

/** Returns an array of sidenote-left distances from the top. */
function getLeftSideNoteOffsets() {
  return getSideNoteOffsets("sidenote-left");
}

/** Returns an array of sidenote-riight distances from the top. */
function getRightSideNoteOffsets() {
  return getSideNoteOffsets("sidenote-right");
}

/**
 * Collects and returns an array of div distances from the top of the page.
 * @param {*} className The name of the div class to get the distance of.
 */
function getSideNoteOffsets(className) {
  var offsets = [];

  $("div." + className).each(function(index) {
    offsets.push($(this).offset().top)
  });

  return offsets;
}

/** Returns the width of the center column. */
function getColumnWidth() {
  return $("div" + _CENTER_COL).outerWidth();
}

/** Returns the width of the left margin sidenotes. */
function getLeftBoxWidth() {
  var leftMargin = getLeftMarginWidth();

  if(leftMargin >= _BOX_MAX_WIDTH){
    return _BOX_MAX_WIDTH;

  } else if(leftMargin <= _BOX_MAX_WIDTH && leftMargin >= _BOX_MIN_WIDTH) {
    return leftMargin;
  }
}

/** Returns the width of the right margin sidenotes. */
function getRightBoxWidth() {
  var rightMargin = getRightMarginWidth();

  if(rightMargin >= _BOX_MAX_WIDTH){
    return _BOX_MAX_WIDTH;

  } else if(rightMargin <= _BOX_MAX_WIDTH && rightMargin >= _BOX_MIN_WIDTH) {
    return rightMargin;
  }
}

/** Returns the width of the left margin. */
function getLeftMarginWidth() {
  return $("div" + _CENTER_COL).offset().left;
}

/** Returns the height of the right margin. */
function getRightMarginWidth() {
  return $(window).width() - (getColumnWidth() + getLeftMarginWidth());
}
