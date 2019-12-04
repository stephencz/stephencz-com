
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

/** Sets the position of the contraster. */
function setContrasterPositions(contrasters, starts, ends) {
  if(contrasters.length !== 0) {
    $.each(contrasters, function(index, value){
      let startMarker = $(starts[index]);
      $(this).css("top", startMarker.offset().top + 30);
    });
  }
}

/** Sets the size of the contraster. */
function setContrasterSizes(contrasters, starts, ends) {
  if(contrasters.length !== 0) {
    $.each(contrasters, function(index, value){
      let startMarker = $(starts[index]);
      let endMarker = $(ends[index]);
      let height = endMarker.offset().top - startMarker.offset().top;
      $(this).css("height", height + 30);
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

  //Create contraster divs
  createContrasters(startMarkers, endMarkers);

  //Get contraster divs
  let contrasters = getContrasters();

  //Set constraster divs positions and size
  setContrasterPositions(contrasters, startMarkers, endMarkers);
  setContrasterSizes(contrasters, startMarkers, endMarkers);

  applyTheme(contrasters, startMarkers, endMarkers);

});

//Positioning after resize
$( window ).on('resize', function(){

  let startMarkers = getContrasterStartMarkers();
  let endMarkers = getContrasterEndMarkers();
  let contrasters = getContrasters();

  setContrasterPositions(contrasters, startMarkers, endMarkers);
  setContrasterSizes(contrasters, startMarkers, endMarkers);


});

$(document).imagesLoaded( function() {
  let startMarkers = getContrasterStartMarkers();
  let endMarkers = getContrasterEndMarkers();
  let contrasters = getContrasters();

  setContrasterPositions(contrasters, startMarkers, endMarkers);
  setContrasterSizes(contrasters, startMarkers, endMarkers);
});