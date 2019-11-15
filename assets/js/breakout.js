
/** Position breakout elements to compensate for container's left margin. */
function setBreakoutPosition() {
  let bodyLeftOffset = $(".body").offset().left;
  $(".breakout").each(function(index, value) {
    let leftOffset = bodyLeftOffset * -1;
    $(this).css("left", leftOffset);
  });
}

// Initial positioning
$( document ).ready(function() {
  setBreakoutPosition();
});

//Positioning after resize
$( window ).on('resize', function(){
  setBreakoutPosition();
});