// ==UserScript==
// @name         Github html preview
// @author       https://github.com/vaniakosmos
// @version      1.0
// @description  Shortcut for htmlpreview.github.io
// @match        https://github.com/**/*.html
// @grant        none
// @require http://code.jquery.com/jquery-3.2.1.min.js
// ==/UserScript==

$(document).ready(function() {
    var viewBtn = $('<a>')
        .attr('href', 'http://htmlpreview.github.io/?' + window.location.href)
        .attr('class', 'btn btn-sm BtnGroup-item')
        .html('Html preview');
    $('.file-actions').find('.BtnGroup').append(viewBtn);
});