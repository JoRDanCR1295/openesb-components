//<!--
// 
// Copyright 2005 by Sun Microsystems, Inc. All rights reserved.
// Use is subject to license terms.
//

// Note: Do not use multiline comments below for TLD examples as renderer XML
// files shall be used to generate Javadoc. Embedding a "*/" in a Javadoc 
// comment cuases compile errors because it terminates the outer comment.

// Set disabled state of table actions. If a selection has been made, actions
// are enabled. If no selection has been made, actions are disabled.
// 
// Note: Use setTimeout when invoking this function. This will ensure that 
// checkboxes and radiobutton are selected immediately, instead of waiting for 
// the onClick event to complete. For example: 
//
// onClick="setTimeout('initAllRows(); disableActions()', 0)"

function disableActions() {
    // Disable table actions by default.
    var table = document.getElementById("form1:table1");
    var selections = table.getAllSelectedRowsCount(); // Hidden & visible selections.
    var disabled = (selections > 0) ? false : true;

    // Set disabled state for top actions.
    document.getElementById("form1:table1:actionsTop:deleteselected").setDisabled(disabled);
 //   document.getElementById("form1:table1:actionsTop:action2").setDisabled(disabled);
 //   document.getElementById("form1:table1:actionsTop:action3").setDisabled(disabled);
 //   document.getElementById("form1:table1:actionsTop:action4").setDisabled(disabled);
//    dropDown_setDisabled("form1:table1:actionsTop:moreActions", disabled);

    // Set disabled state for bottom actions.
//    document.getElementById("form1:table1:actionsBottom:action1").setDisabled(disabled);
//    document.getElementById("form1:table1:actionsBottom:action2").setDisabled(disabled);
//    document.getElementById("form1:table1:actionsBottom:action3").setDisabled(disabled);
 //   document.getElementById("form1:table1:actionsBottom:action4").setDisabled(disabled);
//    dropDown_setDisabled("form1:table1:actionsBottom:moreActions", disabled);
}

//
// Use this function to confirm the number of selected components (i.e., 
// checkboxes or radiobuttons used to de/select rows of the table), affected by
// a delete action. This functionality requires the selectId property of the
// tableColumn component and hiddenSelectedRows property of the tableRowGroup
// component to be set.
// 
// If selections are hidden from view, the confirmation message indicates the
// number of selections not displayed in addition to the total number of
// selections. If selections are not hidden, the confirmation message indicates
// only the total selections.
function confirmDeleteSelectedRows() {
    var table = document.getElementById("form1:table1");
    return table.confirmDeleteSelectedRows();
}

// Use this function to confirm the number of selected components (i.e., 
// checkboxes or radiobuttons used to de/select rows of the table), affected by
// an action such as edit, archive, etc. This functionality requires the 
// selectId property of the tableColumn component and hiddenSelectedRows
// property of the tableRowGroup component to be set.
// 
// If selections are hidden from view, the confirmation message indicates the
// number of selections not displayed in addition to the total number of
// selections. If selections are not hidden, the confirmation message indicates
// only the total selections.
function confirmSelectedRows() {
    var table = document.getElementById("form1:table1");
    return table.confirmSelectedRows("\n\nArchive all selections?");
}

//-->
