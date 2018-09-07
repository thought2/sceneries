"use strict";

// -----------------------------------------------------------------------------
// OBJ
// -----------------------------------------------------------------------------

var OBJFile = require('obj-file-parser');

exports.readWFObj = function(objString) {
    var objFile = new OBJFile(objString);
    var output = objFile.parse();
    console.log(output)
    return output;
}

function main() {
    var canvas = document.getElementById("glcanvas");

    var resize = function() {
        canvas.width = window.innerWidth;
        canvas.height = window.innerHeight;
    }

    resize();
    window.onresize = resize;
}

main();
