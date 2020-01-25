
//==============================================================================
// Table of Contents
//==============================================================================
prefix = 0.0;

//Returns an array of H1 to H6 tags.
function getHeaders() {
  return $("h1, h2, h3, h4, h5, h6");
}

//Creates the table of contents list.
function createTableOfContents() {
  $('.toc-wrapper').append('<div class="toc"><ul></ul></div>');
  if($('.toc-wrapper').hasClass('toc-grow')) {
    $('.toc').addClass('grow');
  }
}

//Populates the table of contents list with entries.
function populateTableOfContents() {
  getHeaders().each(function(index, value) {
    let tag = $(this).prop('tagName').toLowerCase();
    let anchor = $(this).attr('id');
    let text = $(this).text();
    
    incrementTag(tag);

    if(tag == "h1" || tag == "h2" || tag == "h3" || tag == "h4") {
      $('.toc ul').append('<li class="depth-' + tag + '"><a href="#' + anchor + '">' + String(prefix) + ' ' + text + '</a></li>')

    } else {
      $('.toc ul').append('<li class="depth-' + tag + '"><a href="#' + anchor + '">' + text + '</a></li>')

    }
  });
}

function incrementTag(tag) {
  prefix = parseFloat(prefix);

  if(tag == "h1" || tag == "h2") {
    prefix += 1.0;
    prefix = Math.floor(prefix);
    prefix = String(prefix) + ".0";

  } else if(tag == "h3") {
    prefix += 0.1;
    prefix = prefix.toFixed(1);
    prefix = String(prefix);

  } else if(tag == "h4") {
    prefix += 0.01;
    prefix = prefix.toFixed(2);
    prefix = String(prefix);
  }
}

//==============================================================================
// Contraster
//==============================================================================
/** Returns an array of contraster start markers. */
function getContrasterStartMarkers(){
  return $(".contraster-start-marker").toArray()
}

/** Returns an array of contraster end markers. */
function getContrasterEndMarkers() {
  return $(".contraster-end-marker").toArray();
}

/** Returns an array of contraster elements. */
function getContrasters() {
  return $(".contraster").toArray();
}

/** Returns an array of the first element to appear in each contraster. */
function getFirstElements() {
  let $temp = []

  $(getContrasterStartMarkers()).each(function(index, value) {
    return $temp.push($(this).next());
  });

  return $temp;
}

/** Returns an array of the last element to appear in each contraster. */
function getLastElements() {
  let $temp = []

  $(getContrasterEndMarkers()).each(function(index, value) {
    return $temp.push($(this).prev());
  });

  return $temp;
}

/** Sets the position of the contraster. */
function setContrasterPositions(contrasters, starts, ends, firsts, lasts) {
  if(contrasters.length !== 0) {
    $.each(contrasters, function(index, value){
      let startMarker = $(starts[index]);
      let endMarker = $(ends[index]);
      
      let firstElem = $(firsts[index]);
      let lastElem = $(lasts[index]); 

      startMarker.prev().css("margin-bottom", parseInt(startMarker.css("padding-top"), 10));
      firstElem.css("margin-top", 0);
      $(this).css("top", startMarker.offset().top);
    });
  }
}

/** Sets the size of the contraster. */
function setContrasterSizes(contrasters, starts, ends, firsts, lasts) {
  if(contrasters.length !== 0) {
    $.each(contrasters, function(index, value){
      let startMarker = $(starts[index]);
      let endMarker = $(ends[index]);

      let firstElem = $(firsts[index]);
      let lastElem = $(lasts[index]);
      
      endMarker.next().css("margin-top", parseInt(endMarker.css("padding-bottom"), 10));
      lastElem.css("margin-bottom", 0);
      $(this).css("height", (endMarker.offset().top - startMarker.offset().top) + parseInt(endMarker.css("padding-bottom"), 10));

    });
  }
}

function getChildrenRecursively(parent) {
  $found = $();

  if(parent.children().length > 0) {
    $(parent).children().each(function(index, value) {
      $.merge($found, getChildrenRecursively($(this))); 
    });

    $found.push($(parent));
  
  } else {
    $found.push($(parent));

  }

  console.log($found);


  return $.merge([], $found);
}

/** Applies the contraster theme to every element between the start and end markers in the body. */
function applyThemeToBody(contrasters, starts, ends) {
  if(contrasters.length !== 0) {
    $.each(contrasters, function(index, value){
      let startMarker = $(starts[index]);
      let endMarker = $(ends[index]);
      let theme = $(this).attr("class").split(" ")[1];

      var $siblings = startMarker.nextUntil(endMarker);
      var $children = $();
      $siblings.each(function(i2, v2) {
        let more = $(v2).find('*');
        $.merge($children, more);        
      });

      $.merge($siblings, $children).each(function(i2, v2){
        $(v2).addClass(theme);
      });

      startMarker.addClass(theme);
      endMarker.addClass(theme);
    });
  }
}

/** Applies the contraster theme to every sidenote between the start and end markers. */
function applyThemeToSidenotes(contrasters, starts, ends) {
  if(contrasters.length !== 0) {
    $.each(contrasters, function(index, value){

      let theme = $(starts[index]).attr("class").split(" ")[1];

      let topOffset = $(this).offset().top;
      let bottomOffset = topOffset + $(this).height();
      
      $(".sidenote").each(function(index, value){
        let sidenoteOffset = $(value).offset().top;
        let sidenoteHeight = $(value).height();
        let sidenoteBottomOffset = sidenoteOffset + sidenoteHeight;
        if(sidenoteOffset >= topOffset && sidenoteOffset <= bottomOffset) { 
          $(value).addClass(theme);
        } else {
          $(value).removeClass(theme);
        }
      });
      
    });
  }
}

/** Applies the contraster theme. */
function applyTheme(contrasters, starts, ends) {
  applyThemeToBody(contrasters, starts, ends);
  applyThemeToSidenotes(contrasters, starts, ends);
}

/** Creates a contraster for every start and end marker pair. */
function createContrasters(starts, ends)
{

  if(starts.length !== 0) {
    $.each(starts, function(index, value) {
      let contraster = $('<div class="contraster"></div>');
      let contraster_theme = $(this).attr("class").split(" ")[1];
      contraster.addClass(contraster_theme);
      applyLimitedColor($(this), contraster);
      $("body").append(contraster);
    });
  }
}

function applyLimitedColor(start, contraster) {
  if($(start).hasClass("limited-contraster")) {
    $(contraster).css("background-color", $(start).css("background-color"))
  }
}

//==============================================================================
// Breakout
//==============================================================================
/** Position breakout elements to compensate for container's left margin. */
function setBreakoutPosition() {
  if($(".body").length != 0) {
    let bodyLeftOffset = $(".body").offset().left;
    $(".breakout").each(function(index, value) {
      let leftOffset = (bodyLeftOffset) * -1;
      $(this).css("left", leftOffset );
    });
  }
}

//==============================================================================
// Sidenotes
//==============================================================================
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

  let left_col = $(".sn-col-left");
  let toc_wrapper = $(".toc-wrapper");

  if(toc_wrapper.length > 0) {
    left_col.css("top", toc_wrapper.offset().top + toc_wrapper.height() + 42);
  }

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

  //Table of Contents
  createTableOfContents();
  populateTableOfContents();

  //Contraster
  //Get contraster markers
  let startMarkers = getContrasterStartMarkers();
  let endMarkers = getContrasterEndMarkers();

  //First and last elements in each contraster
  let firstElems = getFirstElements();
  let lastElems = getLastElements();

  //Create contraster divs
  createContrasters(startMarkers, endMarkers);

  //Get contraster divs
  let contrasters = getContrasters();

  //Set constraster divs positions and size
  setContrasterPositions(contrasters, startMarkers, endMarkers, firstElems, lastElems);
  setContrasterSizes(contrasters, startMarkers, endMarkers, firstElems, lastElems);

  applyTheme(contrasters, startMarkers, endMarkers);

  //Breakout
  setBreakoutPosition();

  //Sidesnotes
  //Create sidenotes
  createLeftSidenotes();
  createRightSidenotes();

  //Set the sidenote top margins.
  setLeftSidenoteTopMargins();
  setRightSidenoteTopMargins();

});

$(window).resize(function() {
  //Contraster
  let startMarkers = getContrasterStartMarkers();
  let endMarkers = getContrasterEndMarkers();
  let firstElems = getFirstElements();
  let lastElems = getLastElements();
  let contrasters = getContrasters();

  setContrasterPositions(contrasters, startMarkers, endMarkers, firstElems, lastElems);
  setContrasterSizes(contrasters, startMarkers, endMarkers, firstElems, lastElems);

  //Breakout
  setBreakoutPosition();

  //Sidenotes
  setLeftSidenoteTopMargins();
  setRightSidenoteTopMargins();
});

$(document).imagesLoaded( function() {
  //Contraster
  let startMarkers = getContrasterStartMarkers();
  let endMarkers = getContrasterEndMarkers();
  let firstElems = getFirstElements();
  let lastElems = getLastElements();
  let contrasters = getContrasters();

  setContrasterPositions(contrasters, startMarkers, endMarkers, firstElems, lastElems);
  setContrasterSizes(contrasters, startMarkers, endMarkers, firstElems, lastElems);

  //sidenotes
  //Set sidenote margins on resize.
  setLeftSidenoteTopMargins();
  setRightSidenoteTopMargins();

});
