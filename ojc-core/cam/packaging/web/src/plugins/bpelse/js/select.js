//<!--
// 
// Copyright 2005 by Sun Microsystems, Inc. All rights reserved.
// Use is subject to license terms.
//

// Note: Do not use multiline comments below for TLD examples as renderer XML
// files shall be used to generate Javadoc. Embedding a "*/" in a Javadoc 
// comment cuases compile errors because it terminates the outer comment.

// Use this function to initialize all rows displayed in the table when the
// state of selected components change (i.e., checkboxes or radiobuttons used to
// de/select rows of the table). This functionality requires the selectId 
// property of the tableColumn component to be set.
// 
// Note: Use setTimeout when invoking this function. This will ensure that 
// checkboxes and radiobutton are selected immediately, instead of waiting for 
// the onClick event to complete. For example: 
//
// onClick="setTimeout('initAllRows(); disableActions()', 0)"
function initAllRows() {
    // Disable table actions by default.
    var table = document.getElementById("form1:table1");
    table.initAllRows();
}

//-->
