<!--
var req;

function retrieveURL(url) {
	if (window.XMLHttpRequest) { // Non-IE browsers
	    req = new XMLHttpRequest();
	    req.onreadystatechange = processStateChange;
		try {
			netscape.security.PrivilegeManager.enablePrivilege("UniversalBrowserRead");
		} catch (e) {
			alert("Permission UniversalBrowserRead denied.");
		}
		try {
			req.open("GET", url, true);
		} catch (e) {
			alert(e);
		}
		req.send(null);
	} else if (window.ActiveXObject) { // IE
		req = new ActiveXObject("Microsoft.XMLHTTP");
		if (req) {
			req.onreadystatechange = processStateChange;
			req.open("GET", url, true);
			req.send();
		}
	}
}

function processStateChange() {		//alert(req.readyState);
	displayWait();
	if (req.readyState == 4) { // Complete
		if (req.status == 200) { // OK response
			ajaxAction_0();
		} else {
			alert("Problem: " + req.statusText);
		}
	}
}
//=========================================================

function displayWait() {
	if (ajaxPage == "messageTypes") {return;}
	var theTableDiv = document.getElementById("theTableDiv");
	//var waitImg = '<img src="images/wait.gif" width="16" height="16" border="0" alt="">';
	var waitImg = '<img src="images/progressbar.gif" width="55" height="55" border="0" alt="">';
	theTableDiv.innerHTML = waitImg;
}

function ajaxCall(ajaxURL) {
	retrieveURL(ajaxURL);
}

var ajaxPage = "operationsList";
var processorURL = "http://nome.stc.com:8080/aspects/service";

function getWSDL(wsdlURL) {
	ajaxPage = "operationsList";
	var serviceName = "operationsList";
	ajaxCall(processorURL + "?wsdl=" + wsdlURL + "&service=" + serviceName);
}

function ajaxAction_0() {
	if (ajaxPage == "operationsList") {
		rowArr = req.responseText.split(",");		//== COMMENT FOR LOCAL		//alert(rowArr); return;
		//rowArr = inStr.split(",");				//== UNCOMMENT FOR LOCAL
		//for (var i=0; i < rowArr.length; i++) {
		for (var i=0; i < rowArr.length-1; i++) {	//when string ends with coma
			celArr[i] = rowArr[i].split("|");
			var nsArray = celArr[i][0].split(";");
			tableArray[i] = new tableInfoObj(nsArray[0], nsArray[1], nsArray[2], nsArray[3], celArr[i][1]);
		}
		
		var theTableDiv = document.getElementById("theTableDiv");
		theTableDiv.removeChild(theTableDiv.firstChild);
		
		var newTable = document.createElement("TABLE");
		newTable.id = gridTable;
		newTable.border = "0";
		newTable.cellPadding = "4";
		newTable.cellSpacing = "0";
		//newTable.style.margin = "10";
		theTableDiv.appendChild(newTable);
		
		//for (var r=0; r < rowArr.length; r++) {
		for (var r=0; r < rowArr.length-1; r++) {	//when string ends with coma
			addRowToTable(r);
		}
		which = 1;
		//retrieveURL(gcglogintrack);
		initGridTable();
		
		clearCanvas();

	} else if (ajaxPage == "messageTypes") {		//alert(req.responseText);
		document.getElementById("xmlFromAjax").value = req.responseText;		//== COMMENT FOR LOCAL
		document.forms["asp_config_form"].displayXML.value = document.getElementById("xmlFromAjax").value;
	}
}

function addRowToTable(r) {
	var tbl = document.getElementById(gridTable);
	var lastRow = tbl.rows.length;
	// if there's no header row in the table, then iteration = lastRow + 1
	var iteration = lastRow;
	var newRow = tbl.insertRow(lastRow);
	newRow.id = r;
	newRow.title = stripNS(tableArray[r].portType) + " : " + tableArray[r].operName;
	
	/*// add cells
	for (var c=0; c < celArr[r].length; c++) {
		var newCell = newRow.insertCell(c);
		var textNode = document.createTextNode(celArr[r][c]);
			if (textNode.nodeValue == "") {newCell.innerHTML = "&nbsp;";}	//== replace empty cells
		newCell.appendChild(textNode);
		newCell.id = "r_" + iteration + "_c_" + c;
		//newCell.onclick = testId;
	}
	*/
	// add single cell
		var newCell = newRow.insertCell(0);
		var textNode = document.createTextNode(celArr[r][1]);
			if (textNode.nodeValue == "") {newCell.innerHTML = "&nbsp;";}	//== replace empty cells
		newCell.appendChild(textNode);
		newCell.id = "r_" + iteration + "_c_" + 0;
		//newCell.onclick = testId;
}
//------------------------------------------

function fillSelect(box,dbvalue) {
	for (var i = 0; i < box.options.length; i++) {
		if ((box.options[i].value == dbvalue) || (box.options[i].text == dbvalue)) {
			box.selectedIndex = i;
			break;
		}
	}
}

//------------------------------------------
var isLocal = false;

//-->
