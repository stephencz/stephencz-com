
prefix = 0.0;

//Returns an array of H1 to H6 tags.
function getHeaders() {
  return $("h1, h2, h3, h4, h5, h6");
}

//Creates the table of contents list.
function createTableOfContents() {
  $('.toc-wrapper').append('<div class="toc"><ul></ul></div>');
}

//Populates the table of contents list with entries.
function populateTableOfContents() {
  getHeaders().each(function(index, value) {
    let tag = $(this).prop('tagName').toLowerCase();
    let anchor = $(this).attr('id');
    let text = $(this).text();

    console.log($(this));

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

$( document ).ready(function() {

  createTableOfContents();
  populateTableOfContents();

});