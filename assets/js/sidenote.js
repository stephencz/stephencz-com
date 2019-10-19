/** Returns an array of left sidenote markers. */
function getLeftSidenoteMarkers() {
  return $(".sn-marker-left");
}

/** Returns an array of right sidenote markers. */
function getRightSidenoteMarkers() {
  return $(".sn-marker-right");
}

/** Returns an array of sidenotes in the left sidenote column. */
function getLeftSidenotes() {
  return $(".sn-col-left").children(".sidenote");
}

/** Returns an array of sidenotes in the right sidenote column. */
function getRightSidenotes() {
  return $(".sn-col-right").children(".sidenote");
}

function createInnerSidenotes() {
  let left_markers = getLeftSidenoteMarkers();
  let right_markers = getRightSidenoteMarkers();
}

/** Populates the left sidenote column with sidenotes. */
function createLeftSidenotes() {
  let left_col = $(".sn-col-left");
  getLeftSidenoteMarkers().each(function(index, value) {
    left_col.append('<div class="sidenote ">' + $(this).html() + '</div>');
  });
}

/** Populates the right sidenote oclumn with sidenotes. */
function createRightSidenotes() {
  let right_col = $(".sn-col-right");
  getRightSidenoteMarkers().each(function(index, value) {
    right_col.append('<div class="sidenote">' + $(this).html() + '</div>');
  });
}

/** Sets the top margins of sidenotes in the left sidenote column
 *  so that the sidenotes are aligned with their respective markers.
 */
function setLeftSidenoteTopMargins() {
  let markers = getLeftSidenoteMarkers();
  let sidenotes = getLeftSidenotes();

  previous = null
  markers.each(function(index, value) {

    $(sidenotes[index]).css("margin-top", 0);

    marker_offset = $(this).offset().top
    sidenote_offset = $(sidenotes[index]).offset().top;
    margin = marker_offset - sidenote_offset;
    if($(sidenotes[index]).css("visibility") != "hidden" && margin > 0) {
      $(sidenotes[index]).css("margin-top", margin);
    }

    // ADD ADDITIONAL CLASSES
    let marker_classes = String($(markers[index]).attr("class")).replace(/,/g, '').split(/\s+/);
    marker_classes.shift();
    
    $(sidenotes[index]).addClass(marker_classes.join(" "))
  });
}

/** Sets the top margins of the sidenotes in the right sidenote column
 *  so that the sidenotes are aligned with their respective markers.
 */
function setRightSidenoteTopMargins() {
  let markers = getRightSidenoteMarkers();
  let sidenotes = getRightSidenotes();

  previous = null
  
  markers.each(function(index, value) {
    $(sidenotes[index]).css("margin-top", 0);

    marker_offset = $(this).offset().top
    sidenote_offset = $(sidenotes[index]).offset().top;
    margin = marker_offset - sidenote_offset;

    if(margin > 0) {
      $(sidenotes[index]).css("margin-top", margin);
    }

    // ADD ADDITIONAL CLASSES
    let marker_classes = String($(markers[index]).attr("class")).replace(/,/g, '').split(/\s+/);
    marker_classes.shift();

    $(sidenotes[index]).addClass(marker_classes.join(" "))
  });
}

$( document ).ready(function() {

  //Create sidenotes
  createLeftSidenotes();
  createRightSidenotes();

  //Set the sidenote top margins.
  setLeftSidenoteTopMargins();
  setRightSidenoteTopMargins();
});

$( window ).on('resize', function(){

  //Set sidenote margins on resize.
  setLeftSidenoteTopMargins();
  setRightSidenoteTopMargins();
});