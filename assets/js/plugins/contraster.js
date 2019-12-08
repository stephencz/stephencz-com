
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

      // if(parseInt(firstElem.css("margin-top"), 10) > 0) {
        
      //   let offset = lastElem.offset().top - firstElem.offset().top;
      //   let marginFix = parseInt(firstElem.css("margin-top"), 10);
      //   let height =  + lastElem.height() + offset + marginFix;

      //   endMarker.css("padding-bottom", marginFix / 2);
      //   $(this).css("height", height);


      // } else {
      //   let offset = lastElem.offset().top - firstElem.offset().top;
      //   let marginFix = parseInt(firstElem.css("margin-bottom"), 10);
      //   let height =  + lastElem.height() + offset + marginFix;

      //   endMarker.css("padding-bottom", marginFix / 2);
      //   $(this).css("height", height);
      // }

      endMarker.next().css("margin-top", parseInt(endMarker.css("padding-bottom"), 10));
      lastElem.css("margin-bottom", 0);
      $(this).css("height", (endMarker.offset().top - startMarker.offset().top) + parseInt(endMarker.css("padding-bottom"), 10));

    });
  }
}

/** Applies the contraster theme to every element between the start and end markers in the body. */
function applyThemeToBody(contrasters, starts, ends) {
  if(contrasters.length !== 0) {
    $.each(contrasters, function(index, value){
      let startMarker = $(starts[index]);
      let endMarker = $(ends[index]);
      let theme = $(this).attr("class").split(" ")[1];
      startMarker.nextUntil(endMarker).addClass(theme);
      
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
      $("body").append(contraster);
    });
  }
}

// Initial positioning
$( document ).ready(function() {

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

});

//Positioning after resize
$( window ).on('resize', function(){

  let startMarkers = getContrasterStartMarkers();
  let endMarkers = getContrasterEndMarkers();
  let firstElems = getFirstElements();
  let lastElems = getLastElements();
  let contrasters = getContrasters();

  setContrasterPositions(contrasters, startMarkers, endMarkers, firstElems, lastElems);
  setContrasterSizes(contrasters, startMarkers, endMarkers, firstElems, lastElems);


});

$(document).imagesLoaded( function() {
  let startMarkers = getContrasterStartMarkers();
  let endMarkers = getContrasterEndMarkers();
  let firstElems = getFirstElements();
  let lastElems = getLastElements();
  let contrasters = getContrasters();

  setContrasterPositions(contrasters, startMarkers, endMarkers, firstElems, lastElems);
  setContrasterSizes(contrasters, startMarkers, endMarkers, firstElems, lastElems);
});