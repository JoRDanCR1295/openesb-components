<!--
//==	DRAG'n'DROP FUNCTIONS	==//
var selObj;
var selectedDicon;
var tipObj;
var dragFlag = false;
var tipIcon = '<img src="images/wiz-arrow.gif" width="17" height="13" border="0" align="absmiddle" title="Drag action to the canvas"> &nbsp; '
//var tipIcon = '<img src="images/provisor.gif" width="75" height="58" border="0" align="absmiddle" title="Drag action to the canvas"> &nbsp; '
var tipContent = "";

	var dOper = null;
	var dAsp = null;
	var mAsp = null;
	var layerOrder = 100;
	var mouseOffsetX = 10;


function startDrag(e) {
	xEvents(e,1,1);
	//mc_buryPopMenu();
	if (ie) {e = window.event;}
	if (e.button == 2) {return;}

	var selObj = targElem;
	while (selObj.tagName != "TR") {
		selObj = selObj.parentNode;
	}
	dOper = selObj;
	
	if ((e.ctrlKey) || (e.shiftKey)) {return;}
	if (selRowArr.length < 2) {
		doSelect(selObj);
		//mc_selectRow(selObj);
	} else {
		if (selObj.style.backgroundColor == bckgrOver) {
			//selObj.setAttribute("selRow","false");
			doSelectMulti(selObj);
		}
	}

	dragFlag = true;

	tipObj = document.getElementById("tip");
	selectedDicon = tipObj.style;

	tipContent = "";
	for (i=0;i<selRowArr.length;i++) {
		//selRowArr[i].style.visibility = "hidden";
		var nID = selRowArr[i].getAttribute("id");
		//tipContent += tipIcon + tableArray[nID].jTaskType + " - " + tableArray[nID].jCurrentOwner + "<br>";
		tipContent += tipIcon + " - " + tableArray[nID].operName + "<br>";
	}
	//selectedDicon.visibility = "visible";
	document.getElementById("tipSpan").innerHTML = tipContent;

	shiftTo(selectedDicon, mX, mY);
	
	document.body.style.cursor = "move";

	if (selectedDicon) {
		setzIndex(selectedDicon,100);
		return;
	}
	selectedDicon = null;
	return false;
}

function doDrag(e) {
	xEvents(e,1,0);
	if (!dragFlag) {return;}
	if (mAsp) {
		var canvas = document.getElementById("canvasDiv");
		shiftTo(selectedDicon, (mX - canvas.offsetLeft), (mY - canvas.offsetTop));
		// prevent further system response to dragging
		return false;
	}
	if (dAsp) {
		selectedDicon.visibility = "visible";
		dAsp.style.visibility = "hidden";
		var tipHeight = parseInt(selectedDicon.height);
		//shiftTo(selectedDicon, (mX + 10 + document.body.scrollLeft), (mY + document.body.scrollTop - tipHeight));
		shiftTo(selectedDicon, (mX + mouseOffsetX + document.body.scrollLeft), (mY + document.body.scrollTop - tipHeight));
		// prevent further system response to dragging
		return false;
	}
	if (selectedDicon) {
		selectedDicon.visibility = "visible";
		for (i=0;i<selRowArr.length;i++) {
			selRowArr[i].style.visibility = "hidden";
		}
		var tipHeight = parseInt(selectedDicon.height);
		//shiftTo(selectedDicon, (mX + 10 + document.body.scrollLeft), (mY + document.body.scrollTop - tipHeight));
		shiftTo(selectedDicon, (mX + mouseOffsetX + document.body.scrollLeft), (mY + document.body.scrollTop - tipHeight));
		// prevent further system response to dragging
		return false;
	}
}

function shiftTo(dicon, x, y) {
	dicon.left = x + "px";
	dicon.top = y + "px";
}

function setzIndex(dicon, zOrder) {
	if (selectedDicon) {
		dicon.zIndex = zOrder;
	}
}

function endDrag(e) {
	xEvents(e,1,1);
	if (!dragFlag) {return;}
	dragFlag = false;
	var tipHeight = parseInt(selectedDicon.height);
	

	releaseDicon();
	
		if (mAsp) {
			//alert(targElem.tagName+"\n"+targElem.getAttribute("dropID"));
			dropMovedAspect(targElem.getAttribute("dropID"), (mX + mouseOffsetX + document.body.scrollLeft), (mY + document.body.scrollTop - tipHeight));
			return false;
		}
	if (targElem.getAttribute("dropID")) {
		if (dAsp) {
			dropAspect(targElem.getAttribute("dropID"), (mX + mouseOffsetX + document.body.scrollLeft), (mY + document.body.scrollTop - tipHeight));
			return;
		}
		if (dOper) {
			dropOperation();
			dOper = null;
		}
	}
}

function releaseDicon() {
	if (mAsp) {
		document.body.style.cursor = "";
		selectedDicon = null;
		//mAsp = null;
		return false;
	}
	if (dAsp) {
		selectedDicon.visibility = "hidden";
		dAsp.style.visibility = "visible";
		document.body.style.cursor = "";
		selectedDicon = null;
		//dAsp = null;
		return false;
	}
	if (selectedDicon) {
		selectedDicon.visibility = "hidden";
		for (i=0;i<selRowArr.length;i++) {
			selRowArr[i].style.visibility = "visible";
		}
		setzIndex(selectedDicon, 0);
	}
	document.body.style.cursor = "";
	selectedDicon = null;
	tipObj = null;
}

document.onmousemove = doDrag;
document.onmouseup = endDrag;

//----------------------------------------------------------------


function startDragAspect(e) {
	if (!operIsSet) {
		alert("Please choose operation first.");
		return;
	}
	xEvents(e,1,1);
	if (ie) {e = window.event;}
	if (e.button == 2) {return;}

	var selObj = targElem;
	dAsp = selObj;
	dragFlag = true;

	tipObj = document.getElementById("tipAsp");
	selectedDicon = tipObj.style;
	tipContent = "";
		tipContent += '<img src="'+selObj.src+'" width="30" height="34" border="0">';
	document.getElementById("tipSpanAsp").innerHTML = tipContent;
	shiftTo(selectedDicon, mX, mY);
	
	document.body.style.cursor = "move";

	if (selectedDicon) {
		setzIndex(selectedDicon,100);
		return;
	}
	selectedDicon = null;
	return false;
}

function startMoveAspect(e) {
	xEvents(e,1,1);
	if (ie) {e = window.event;}
	if (e.button == 2) {return;}

	var selObj = targElem;
	mAsp = selObj;
	dragFlag = true;
	selectedDicon = mAsp.style;	
	document.body.style.cursor = "move";

	if (selectedDicon) {
		setzIndex(selectedDicon,layerOrder++);
		return;
	}
	selectedDicon = null;
	return false;
}

//-->
