<!--

//==	INITIALIZE GLOBALS	==//
var bckgrOn = "#FBE797";
var bckgrOff = "#FFFFFF";
var bckgrOver = "#EEFFCC";
var bckgrOut = "";

var gridTable = "gridTable";
var rowsBegin = 0;	// = 1;
var rowsEnd = 0;	// = 1;

	var gridStart = 20;
	var gridTop = 98;
	var gridVert = 100;
	var gridSize = 100;
	var chainEnd = gridStart + gridSize;
	var operIsSet = false;

var aRows;
var selRowArr = new Array();
var prevSelRow = null;

window.onload=initGridTable;


//==	XB FUNCTIONS	==//
var NS = (document.implementation.createDocument) ? true : false;
var ie = document.all;
var e, targElem, mX, mY;

function xEvents(e,cancelDef,cancelBub) {
	if (ie) {e = window.event;}
	targElem = (ie)?e.srcElement:e.target;
	mX = e.clientX;// - parseInt(this.style.left);
	mY = e.clientY;// - parseInt(this.style.top);
	if (!ie && cancelDef) {
		e.preventDefault();
	}
	if (cancelBub) {
		if (ie) {
			e.cancelBubble = true;
		} else {
			e.stopPropagation();
		}
	}
}

//==	KEY EVENT CAPTURE	==//
var deleteKey = 46;
var arrUpKey = 38;
var arrDnKey = 40;

function getKeycode(e) {
	if (ie) {e = window.event;}
	var keyDown = (ie)?e.keyCode:e.which;			//alert(keyDown);
	//window.status = "Keycode = " + keyDown;
    if (keyDown == deleteKey) {							// Check for Delete key
        if (selRowArr.length != 0) {
            rowDeleteAction();
        }
    }
    if (keyDown == arrDnKey) {							// Check for Arrow key
        if (selRowArr.length != 0) {
			var lastRowId = parseInt(selRowArr[selRowArr.length-1].id);
			var nextRowId = lastRowId + 1;
			if (nextRowId >= (aRows.length-1-rowsEnd)) {return;}
			var nextRow = document.getElementById(nextRowId);
			selectRow(e,nextRow);
        }
    }
    if (keyDown == arrUpKey) {							// Check for Arrow key
        if (selRowArr.length != 0) {
			var lastRowId = parseInt(selRowArr[selRowArr.length-1].id);
			var nextRowId = lastRowId - 1;
			if (nextRowId < 0) {return;}
			var nextRow = document.getElementById(nextRowId);
			selectRow(e,nextRow);
        }
    }
}
document.onkeydown = getKeycode;


//==	INIT FUNCTION	==//
function initGridTable() {
	if (!document.getElementById(gridTable)){return;}
	unSelectAll();
	var oTable = document.getElementById(gridTable);
	oTable.style.backgroundColor = "transparent";
	aRows = oTable.rows;		

	for (var k=rowsBegin; k < aRows.length-rowsEnd; k++) {
		//if (tableArray.length > 0) {
		//	rowID = tableArray[k-rowsBegin];
		//} else {
			rowID = k-rowsBegin;
		//}
		aRows[k].setAttribute("id", rowID);
		aRows[k].style.backgroundColor = "transparent";
		aRows[k].className = "row";
		aRows[k].onmouseover = rowOver;
		aRows[k].onmouseout = rowOut;
		aRows[k].onclick = clickRow;
		aRows[k].ondblclick = rowAction;
		aRows[k].oncontextmenu = contextmenuActions;
		aRows[k].onmousedown = startDrag;

		for (var c=0; c < aRows[k].childNodes.length; c++) {
			if (aRows[k].childNodes[c].nodeType == "1") {
				aRows[k].childNodes[c].unselectable = "on";		///////======	CHANGE for NS events	=======
				aRows[k].childNodes[c].style.backgroundColor = "transparent";
				if (aRows[k].childNodes[c].innerHTML == "") {		//== replace empty cells with "&nbsp;"
					aRows[k].childNodes[c].innerHTML = "&nbsp;";
				}
			}
		}

	}
	document.body.focus();
	customInit();
}


function rowOver(e) {
	if (operIsSet) {return;}
	xEvents(e,1,1);
	var evtRow = targElem;
	while (evtRow.tagName != "TR") {
		evtRow = evtRow.parentNode;
	}
	if (evtRow.getAttribute("selRow") == "true") {return;}
	//evtRow.className = "rowOver";
	childrenCount = evtRow.childNodes;
	for (i=0;i<childrenCount.length;i++) {
		if (childrenCount[i].tagName == "TD") {
			evtRow.childNodes[i].style.backgroundColor = bckgrOver;
		}
	}
}
function rowOut(e) {
	if (operIsSet) {return;}
	xEvents(e,1,1);
	var evtRow = targElem;
	while (evtRow.tagName != "TR") {
		evtRow = evtRow.parentNode;
	}
	if (evtRow.getAttribute("selRow") == "true") {return;}
	//evtRow.className = "row";
	childrenCount = evtRow.childNodes;
	for (i=0;i<childrenCount.length;i++) {
		if (childrenCount[i].tagName == "TD") {
			evtRow.childNodes[i].style.backgroundColor = bckgrOut;
		}
	}
}


//==	ROW CLICK FUNCTIONS	==//

function clickRow(e) {
	if (operIsSet) {return;}
	xEvents(e,1,1);
	var evtRow = targElem;
	while (evtRow.tagName != "TR") {
		evtRow = evtRow.parentNode;
	}									//alert(evtRow.id); return;
	selectRow(e,evtRow);
}

function selectRow(e,rowObj) {			//alert(rowObj.rowIndex);
	if (operIsSet) {return;}					//== Prevent select when operation already into canvas
	if (ie) {e = window.event;}
	if (e.ctrlKey) {							return;	//== TEMP DISABLE MULTI-SELECT
		doSelectMulti(rowObj);
	} else if (e.shiftKey) {					return;	//== TEMP DISABLE MULTI-SELECT
		if (prevSelRow) {
			doSelectCont(rowObj);
		} else {
			doSelect(rowObj);
		}
	} else {
		doSelect(rowObj);				//alert(rowObj.id);
	}
}

function doSelect(rowObj) {
	unSelectAll();
	childrenCount = rowObj.childNodes;
	for (i=0;i<childrenCount.length;i++) {
		if (childrenCount[i].tagName == "TD") {
			rowObj.childNodes[i].style.backgroundColor = bckgrOn;
		}
	}
//	rowObj.className = "rowSel";
	rowObj.setAttribute("selRow","true");
	
	selRowArr[selRowArr.length] = rowObj;
	prevSelRow = rowObj;
	selectAction();
}

function doSelectCont(rowObj) {
	unSelectAll();
	var rowMin = (prevSelRow.rowIndex < rowObj.rowIndex)?prevSelRow.rowIndex:rowObj.rowIndex;
	var rowMax = (prevSelRow.rowIndex > rowObj.rowIndex)?prevSelRow.rowIndex:rowObj.rowIndex;
	//unSelect(prevSelRow);
	for (var n=rowMin; n <= rowMax; n++) {
		doSelectMulti(aRows[n]);
	}
	//prevSelRow = rowObj;
}

function doSelectMulti(rowObj) { //alert(rowObj.getAttribute("selRow"));
	if (rowObj.getAttribute("selRow") == "true") {
		unSelect(rowObj);
		return;
	}
	childrenCount = rowObj.childNodes;
	for (i=0;i<childrenCount.length;i++) {
		if (childrenCount[i].tagName == "TD") {
			rowObj.childNodes[i].style.backgroundColor = bckgrOn;
		}
	}
//	rowObj.className = "rowSel";
	rowObj.setAttribute("selRow","true");
	
	selRowArr[selRowArr.length] = rowObj;
	prevSelRow = rowObj;
	selectAction();
}
 
function unSelect(lastSelRow) {
	if (selRowArr.length > 0) {
		childrenCount = lastSelRow.childNodes;
		for (i=0;i<childrenCount.length;i++) {
			if (childrenCount[i].tagName == "TD") {
				lastSelRow.childNodes[i].style.backgroundColor = bckgrOff;
				lastSelRow.setAttribute("selRow","false");
			}
		}
//		lastSelRow.className = "row";
		lastSelRow.setAttribute("selRow","false");
		for (j=0;j<selRowArr.length;j++) {
			if (selRowArr[j] == lastSelRow) {
				deletedNode = j;
			}
		}
		selRowArr.splice(deletedNode, 1);
		unSelectAction();
	}
}
 
function unSelectAll() {
	for (j=0;j<selRowArr.length;j++) {
		lastSelRow = selRowArr[j];
		childrenCount = lastSelRow.childNodes;
		for (i=0;i<childrenCount.length;i++) {
			if (childrenCount[i].tagName == "TD") {
				lastSelRow.childNodes[i].style.backgroundColor = bckgrOff;
				lastSelRow.setAttribute("selRow","false");
			}
		}
//		lastSelRow.className = "row";
		lastSelRow.setAttribute("selRow","false");
	}
	selRowArr.length = 0;
	unSelectAction();
}

function selectAll() {
	if (selRowArr.length == 0) {
		unSelectAll();
		for (var n=rowsBegin; n < aRows.length-rowsEnd; n++) {
			doSelectMulti(aRows[n]);
		}
	} else {
		unSelectAll();
	}
}

function doNothing() {
}

//////////////////////////////////////////////////////////////////////////////////////////

	function selectAction() {				//==	ENVOKED ON ROW CLICK
		/*
	    if (selRowArr[0]) {		//alert(selRowArr[0].getAttribute("id"));return;
			var nID = selRowArr[0].getAttribute("id");
			taskId = tableArray[nID].jTaskId;
			curOwnr = tableArray[nID].jCurrentOwner;
			creator = tableArray[nID].jCreator;
			stage = tableArray[nID].jStage;
		}
		checkButtons();
		populateHidden();
		showDetails();
		*/
		doNothing();
	}
	
	function unSelectAction() {				//==	ENVOKED WHEN NO ROWS SELECTED
		/*
		checkButtons();
		taskId = null;
		curOwnr = null;
		creator = null;
		stage = 0;
		clearHidden();
		showDetails();
		document.getElementById("actionMsg").innerHTML = "";
		*/
		doNothing();
	}
	
		//==	ASSIGN ROW ACTIONS or "doNothing()"	==//
	
	function rowAction() {					//==	ENVOKED ON ROW DOUBLE-CLICK
		/*
	    if (selRowArr[0]) {
			//alert("rowAction: " + selRowArr[0].getAttribute("id"));
			var nID = selRowArr[0].getAttribute("id");
			//alert("rowAction:\n\ntaskID = " + tableArray[nID].jTaskId + "\ncurrentOwn = " + tableArray[nID].jCurrentOwner + "\ncreator = " + tableArray[nID].jCreator + "\nstage = " + tableArray[nID].jStage + "\ntaskType = " + tableArray[nID].jTaskType);
			doView();
		}
		*/
		doNothing();
	}
	
		//==	ASSIGN ADDITIONAL INIT ACTIONS or "doNothing()"	==//
	
	function customInit() {					//==	ENVOKED ON WINDOW ONLOAD EVENT
		//toolbarInit();
		doNothing();
	}

//////////////////////////////////////////////////////////////////////////////////////////

	function contextmenuActions(e) {	//	==	RESERVED
		xEvents(e,1);
			////showObjMenu(e);
		////alert("oncontextmenu: " + targElem.tagName + "\nx = " + mX + ", y = " + mY);
			////if (!ieDefault) {return false;}
		clickRow(e);
		////alert("rightClick: " + selRowArr[0].getAttribute("id") + "\ntaskID = " + taskId);
		/*
		var nID = selRowArr[0].getAttribute("id");
		alert("oncontextmenu:\n\ntaskID = " + tableArray[nID].jTaskId + "\ncurrentOwn = " + tableArray[nID].jCurrentOwner + "\ncreator = " + tableArray[nID].jCreator + "\nstage = " + tableArray[nID].jStage + "\ntaskType = " + tableArray[nID].jTaskType);
		*/
		return false;
		//doNothing();
	}

	function rowDeleteAction() {			//==	ENVOKED ON PRESS DELETE KEY OR BUTTON	==	RESERVED
		/*
	    if (selRowArr[0]) {
			////alert("rowDeleteAction: " + selRowArr[0].getAttribute("id"));
			for (j=0;j<selRowArr.length;j++) {
				var nID = selRowArr[j].getAttribute("id");
				alert("rowDeleteAction:\n\ntaskID = " + tableArray[nID].jTaskId + "\ncurrentOwn = " + tableArray[nID].jCurrentOwner + "\ncreator = " + tableArray[nID].jCreator + "\nstage = " + tableArray[nID].jStage + "\ntaskType = " + tableArray[nID].jTaskType);
			}
		}
		*/
		doNothing();
	}

//////////////////////////////////////////////////////////////////////////////////////////

//-->
