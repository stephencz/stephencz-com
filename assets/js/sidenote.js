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
  let markers = getLeftSidenoteMarkers();
  let left_col = $(".sn-col-left");

  getLeftSidenoteMarkers().each(function(index, value) {
    let sidenote = $('<div class="sidenote ">' + $(this).html() + '</div>')
    left_col.append(sidenote);

    let marker_classes = String($(markers[index]).attr("class")).replace(/,/g, '').split(/\s+/);
    marker_classes.shift();
    
    sidenote.addClass(marker_classes.join(" "));
    sidenote.attr("style", $(this).attr("style"));
  });
}

/** Populates the right sidenote oclumn with sidenotes. */
function createRightSidenotes() {
  let markers = getRightSidenoteMarkers();
  let right_col = $(".sn-col-right");

  getRightSidenoteMarkers().each(function(index, value) {
    let sidenote = $('<div class="sidenote ">' + $(this).html() + '</div>')
    right_col.append(sidenote);

    let marker_classes = String($(markers[index]).attr("class")).replace(/,/g, '').split(/\s+/);
    marker_classes.shift();

    sidenote.addClass(marker_classes.join(" "));
    sidenote.attr("style", $(this).attr("style"));

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

    let margin = 0;

    if(previous != null) {
      previousBottom = previous.offset().top + previous.height();
      markerOffset = $(this).offset().top;
      margin = markerOffset - previousBottom;

    } else {
      margin = $(this).offset().top - $(".sn-col-left").offset().top;
    }

    if($(sidenotes[index]).css("visibility") != "hidden" && margin > 0) {
      $(sidenotes[index]).css("margin-top", margin);
    }

    previous = $(sidenotes[index]);
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

    let margin = 0;

    if(previous != null) {

      previousBottom = previous.offset().top + previous.height();
      markerOffset = $(this).offset().top;
      margin = markerOffset - previousBottom;

    } else {
      margin = $(this).offset().top - $(".sn-col-right").offset().top;
    }

    if($(sidenotes[index]).css("visibility") != "hidden" && margin > 0) {
      $(sidenotes[index]).css("margin-top", margin);
    }

    previous = $(sidenotes[index]);
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

$(document).imagesLoaded( function() {
  //Set sidenote margins on resize.
  setLeftSidenoteTopMargins();
  setRightSidenoteTopMargins();


});

// /**
//  * Determines if the first element in overlapping the second element.
//  * @param {*} e1 
//  * @param {*} e2 
//  */
// function isYOverlap(e1, e2) {
//   let firstTopOffset = $(e1).offset().top;
//   let firstHeight = $(e1).height();

//   let secondTopOffset = $(e2).offset().top;

//   if(!(firstTopOffset >= secondTopOffset)) {
//     if((firstTopOffset + firstHeight) >= secondTopOffset) {
//       return true;
//     }
//   }

//   return false;
// }

// /**
//  * Determines if an element which exists within another, for example a sidenote
//  * inside a contraster, is overlapping outside of the it (sidenote leaking out of
//  * the bottom of the contraster for example).
//  * @param {*} e1 
//  * @param {*} e2 
//  */
// function isYExitLap(e1, e2) {
//   let firstTopOffset = $(e1).offset().top;
//   let firstHeight = $(e1).height();

//   let secondTopOffset = $(e2).offset().top;
//   let secondHeight = $(e2).height();

//   if(((firstTopOffset + firstHeight) >= (secondTopOffset + secondHeight)) && firstTopOffset < (secondTopOffset + secondHeight)) {
//     return true;
//   }

//   return false;
// }

// function getPreOverlapHeight() {

// }

// function getPostExitlapHeight() {
  
// }


// function fixOverlap(sidenotes) {

//   //Arrays of the things a sidenote could overlap
//   let contrasters = $(".contraster");

//   //Merge the potential victims 
//   //NOT NEEDED YET

//   //Check for overlap and handle it.
//   sidenotes.each(function(i1, v1) {
//     contrasters.each(function(i2, v2) {
//       if(isYOverlap(sidenotes[i1], contrasters[i2]))
//       {
//         $(sidenotes[i1]).css("color", "#0f0");
//       }

//       if(isYExitLap(sidenotes[i1], contrasters[i2]))
//       {
//         $(sidenotes[i1]).css("color", "#0f0");
//       }
//     });
//   });
// }