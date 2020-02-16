
function setBreakoutPosition() {
  if($(".body").length != 0) {
    let bodyLeftOffset = $(".body").offset().left;
    $(".breakout").each(function(index, value) {
      let leftOffset = (bodyLeftOffset) * -1;
      $(this).css("left", leftOffset );
    });
  }
}

$( document ).ready(function() {

  //Breakout
  setBreakoutPosition();

});

$(window).resize(function() {

  //Breakout
  setBreakoutPosition();

});
