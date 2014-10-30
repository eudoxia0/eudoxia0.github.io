function fixCodeClasses(block) {
    /* Jekyll emits <code> tags that have classes like 'language-lisp' or
       'language-markdown'. highlight.js allegedly supports those. This isn't
       the case. So this function removes the 'language-' prefix. */
    const new_class = $(block).attr('class').replace('language-', '');
    $(block).attr('class', new_class);
}

function highlightEverything(block) {
    hljs.highlightBlock(block);
}


$(document).ready(function() {
    $('pre code').each(function(i, block) {
        fixCodeClasses(block);
        highlightEverything(block);
    });
});
