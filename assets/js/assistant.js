/** Initializes the assistant bar by hiding it, and then setting its visibility to visible. */
function initializeAssistant() {
  $(".assistant-wrapper").hide();
  $(".assistant-wrapper").css("visibility", "visible");
}

/**
 * Determines whether or not the assistant should be shown.
 * @return {*} True when the assistant should be shown. Otherwise false.
 */
function shouldAssistantBeShown() {
  let toc = $(".toc-wrapper");
  let tocOffset = toc.offset().top + toc.height();
  if($(window).scrollTop() >= tocOffset) {
    return true;
  }

  return false;
}

/**
 * Shows the assistant.
 * @param {*} speed The speed of the show transition.
 */
function showAssistant(speed) {
  $(".assistant-wrapper").slideDown(speed);
}

/**
 * Hides the assistant.
 * @param {*} speed The speed of the hide transition.
 */
function hideAssistant(speed) {
  $(".assistant-wrapper").slideUp(speed);
}

//When the document is safe to be manipulated.
$(document).ready( function() {
  initializeAssistant();
});

//When the document is scrolled.
$(document).scroll( function() {
  if(shouldAssistantBeShown(540)) {
    showAssistant(100);

  } else {
    hideAssistant(100);

  }
});