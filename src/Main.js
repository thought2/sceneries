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
