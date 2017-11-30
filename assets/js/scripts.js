function fixCodeClasses(block) {
  /* Jekyll emits <code> tags that have classes like 'language-lisp' or
     'language-markdown'. highlight.js allegedly supports those. This isn't the
     case. So this function removes the 'language-' prefix. */
  const old_class = $(block).attr('class');
  if (old_class) {
    const new_class = old_class.replace('language-', '');
    $(block).attr('class', new_class);
  }
}

function highlightEverything() {
  /* Highlight every code block */
  $('pre code').each(function(i, block) {
    console.log(block);
    fixCodeClasses(block);
    hljs.highlightBlock(block);
  });
}

function linkImages() {
  /* Wrap all images in links to their source */
  $('article img').each( function() {
    const source = $(this).attr('src');
    $(this).wrap('<a href="' + source + '"></a>');
  });
}

$(document).ready(function() {
  highlightEverything();
  linkImages();
});
