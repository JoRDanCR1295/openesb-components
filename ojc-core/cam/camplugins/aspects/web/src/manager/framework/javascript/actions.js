<!--

var oImgProv = new Image(30,37);
oImgProv.src = "images/provisor.gif";
var oImgCons = new Image(30,37);
oImgCons.src = "images/consumer.gif";

var rowBodyDisplay = (NS)?"table-row-group":"block";
var selectedAspect = null;


function stripNS(strNS) {
	var remark = /\{.+?\}/g;
	strNS = strNS.replace(remark,"");
	//strNS = strNS.replace( new RegExp( "\{.+?\}", "g" ), "" );
	return strNS;
}

function dropOperation() {
	var canvas = document.getElementById("canvasDiv");
    if (selRowArr[0]) {
		var connDiv = document.createElement("DIV");
		canvas.appendChild(connDiv);
		connDiv.id = "connLine";
		connDiv.style.position = "absolute";
		connDiv.style.left = "35px";
		connDiv.style.top = gridTop + oImgProv.height/2 + "px";
		connDiv.style.width = gridSize + "px";
		connDiv.style.height = "2px";
		connDiv.style.backgroundColor = "#CC0000";
		connDiv.innerHTML = '<img src="images/pix.gif" id="connIMG" width="'+gridSize+'" height="1" border="0">';

		for (j=0;j<selRowArr.length;j++) {
			var nID = selRowArr[j].getAttribute("id");
			//alert("dropOperation:\nnID=" + nID + "\nportType=" + tableArray[nID].portType + "\noperation=" + tableArray[nID].operName + "\nX=" + mXd + "\nY=" + mYd);

			var newProvisor = oImgProv.cloneNode(true);
			canvas.appendChild(newProvisor);
			newProvisor.id = tableArray[nID].portType + ":" + tableArray[nID].operName;
			newProvisor.style.position = "absolute";
			newProvisor.style.left = gridStart;
			newProvisor.style.top = gridTop;
			//newProvisor.title = "portType: " + stripNS(tableArray[nID].portType) + ", operation: " + tableArray[nID].operName;
			newProvisor.setAttribute("title","portType: " + stripNS(tableArray[nID].portType) + ", operation: " + tableArray[nID].operName);
			chainArray[chainArray.length++] = new chainObj(newProvisor.id, gridStart, gridTop, 0, "", "", "", "");
			chainCount++;

			var newConsumer = oImgCons.cloneNode(true);
			canvas.appendChild(newConsumer);
			newConsumer.id = "Consumer";
			newConsumer.style.position = "absolute";
			newConsumer.style.left = gridStart + gridSize;		//	gridSize*chainSize
			newConsumer.style.top = gridTop;
			newConsumer.setAttribute("title","Consuming EndPoint");
			chainArray[chainArray.length++] = new chainObj("Consumer", gridStart + gridSize, gridTop, 0, "", "", "", "");
			chainCount++;

			var operNameDiv = document.createElement("DIV");
			canvas.appendChild(operNameDiv);
			operNameDiv.innerHTML = tableArray[nID].operName;
			operNameDiv.style.position = "absolute";
			operNameDiv.style.left = "10px";
			operNameDiv.style.top = gridTop + newProvisor.offsetHeight;
			operNameDiv.style.paddingTop = "10px";
			
			var serviceNameDisplay = stripNS(tableArray[nID].serviceName);
			
			document.getElementById("serviceNameSpan").innerHTML = stripNS(tableArray[nID].serviceName);
			document.getElementById("portTypeNameSpan").innerHTML = stripNS(tableArray[nID].portType);
			document.getElementById("operNameSpan").innerHTML = tableArray[nID].operName;
			
			var operExpTA = document.createElement("TEXTAREA");
			document.getElementById("exportDiv").appendChild(operExpTA);
			operExpTA.id = "export_operation";
			operExpTA.name = "export_operation";

			operationXMLstr = "";
			operationXMLstr += '<?xml version="1.0" encoding="UTF-8"?>\n';
			operationXMLstr += '<aspectmap>\n';
				operationXMLstr += '\t<aspect exchangeType = "filterRequestReply" ID="1">\n';
					operationXMLstr += '\t\t<input serviceName="{foo}pl1"\n';
						operationXMLstr += '\t\t\tportName="server"\n';
						operationXMLstr += '\t\t\tportType="portType"\n';
						operationXMLstr += '\t\t\toperation="operation"\n';
						operationXMLstr += '\t\t\tmessageType="replyMessageType"\n';
						operationXMLstr += '\t\t\tfile="input.xsl"\n';
						operationXMLstr += '\t\t\ttransformJBI="false" />\n';
					operationXMLstr += '\t\t<output ID="1"\n';
						operationXMLstr += '\t\t\tserviceName="'+tableArray[nID].serviceName+'"\n';
						operationXMLstr += '\t\t\tportName="'+tableArray[nID].portName+'"\n';
						operationXMLstr += '\t\t\tportType="'+tableArray[nID].portType+'"\n';
						operationXMLstr += '\t\t\toperation="'+tableArray[nID].operName+'"\n';
						operationXMLstr += '\t\t\tmessageType="replyMessageType"\n';
						operationXMLstr += '\t\t\tfile="output.xsl"\n';
						operationXMLstr += '\t\t\ttransformJBI="false" />\n';
						
				//operationXMLstr += '\t</aspect>\n';		//== ADD LATER ON EXPORT
			//operationXMLstr += '</aspectmap>\n';			//== ADD LATER ON EXPORT
			
			operExpTA.value = operationXMLstr + operationXMLstrEnd;
					//operExpTA.title = operExpTA.id;		//== FOR DEBUG ONLY

			//==	MAKE AJAX CALL FOR OPERATION RULES
			ajaxPage = "messageTypes";
			var serviceName = "messageTypes";
			/*
			ajaxCall(processorURL + "?wsdl=" + document.getElementById("wsdlURL").value + "&service=" + serviceName + "&portType=" + celArr[nID][0] + "&operation=" + celArr[nID][1]);				//== COMMENT FOR LOCAL
			//ajaxAction_0();									//== UNCOMMENT FOR LOCAL
			*/
			if (isLocal) {
				ajaxAction_0();
			} else {
				ajaxCall(processorURL + "?wsdl=" + document.getElementById("wsdlURL").value + "&service=" + serviceName + "&portType=" + celArr[nID][0] + "&operation=" + celArr[nID][1]);
			}
		}

		operIsSet = true;
		for (var k=rowsBegin; k < aRows.length-rowsEnd; k++) {
			aRows[k].onmousedown = doNothing;		//- prevent another action on canvas
		}
	}
	unSelectAll();
}

function dropAspect(dropID, mXd, mYd) {
	var canvas = document.getElementById("canvasDiv");
	var dropPosX = mXd - canvas.offsetLeft;
	if ((dropPosX > chainArray[1].cX) || (dropPosX < chainArray[0].cX)) {return;}		//- prevent drop outside the chain
    if (dAsp) {
		var m = chainArray.length;

		var newAsp = dAsp.cloneNode(false);
		canvas.appendChild(newAsp);
		newAsp.style.position = "absolute";
		newAsp.style.left = dropPosX;
		newAsp.style.top = mYd - canvas.offsetTop;
		newAsp.onmousedown = startMoveAspect;
		newAsp.onclick = selectAspect;
		newAsp.id = dAsp.id + "_" + chainCount;
							//alert(dAsp.style.top); return;
		snapToGrid(newAsp);
		var oLeft = parseInt(newAsp.style.left);
		var oTop = parseInt(newAsp.style.top);
		chainArray[chainArray.length++] = new chainObj(newAsp.id, oLeft, oTop, 0, "", "", "", "");
		
		chainArray[m].p0 = configArray[dAsp.id][0][1];
		chainArray[m].p1 = configArray[dAsp.id][1][1];
		chainArray[m].p2 = configArray[dAsp.id][2][1];

		var aspExpDiv = document.createElement("TEXTAREA");
		document.getElementById("exportDiv").appendChild(aspExpDiv);
		aspExpDiv.id = "export_"+newAsp.id;
		aspExpDiv.name = "export_"+newAsp.id;
		//aspExpDiv.value = aspExpDiv.id;
		
		exportConfig(m,dAsp.id,newAsp.id);

		dAsp = null;
		
		chainCount++;
		reArrange(dropPosX, 1);
	}
}

function dropMovedAspect(dropID, mXd, mYd) {
    if (mAsp) {
		for (i=0;i<chainArray.length;i++) {
			if (mAsp.id == chainArray[i].cID) {
				var oldX = chainArray[i].cX;
				break;
			}
		}
		
		var canvas = document.getElementById("canvasDiv");
		var dropPosX = mXd - canvas.offsetLeft;
		if (((dropPosX > chainArray[1].cX) || (dropPosX < chainArray[0].cX)) || !dropID) {		//- prevent move outside the chain
			mAsp.style.left = chainArray[i].cX + "px";
			mAsp.style.top = chainArray[i].cY + "px";
		}
		
		reArrange(dropPosX-mouseOffsetX, 0, oldX); //alert(dropPosX);return;
		mAsp = null;
	}
}

//////////////////////////////////////////////////////

function selectAspect(e) {
	deSelectAspect();
	xEvents(e,1,1);
	if (ie) {e = window.event;}
	if (e.button == 2) {return;}
	selectedAspect = targElem;
	//selObj.style.backgroundColor = "#FF0000";
	selectedAspect.style.border = "2px solid #0000FF";
	showAspectProps(1);
}

function deSelectAspect() {
	if (selectedAspect) {
		selectedAspect.style.border = "0px solid #0000FF";
	}
	selectedAspect = null;
	showAspectProps();
}

function showAspectProps(vis) {
	var canvas = document.getElementById("canvasDiv");
	var propsBox = document.getElementById("propsDiv");
	var propsText = document.getElementById("propsSpan");
	if (vis) {
		if (selectedAspect) {
			for (var n=0;n<chainArray.length;n++) {
				if (chainArray[n].cID == selectedAspect.id) {
					break;
				}
			}
		}
		propsBox.style.marginLeft = canvas.offsetLeft-parseInt(document.getElementById("bodyID").style.marginLeft) + "px";
		propsText.innerHTML = "Type: <b>" + selectedAspect.title;
		propsText.innerHTML += "</b> &nbsp; &nbsp; ID: " + selectedAspect.id;
		//propsText.innerHTML += "<br>X: " + selectedAspect.style.left;
		//propsText.innerHTML += "<br>Y: " + selectedAspect.style.top;
		//propsText.innerHTML += "<br>Branch: " + chainArray[n].branch;
		//propsText.innerHTML += "<br>Order: " + Math.floor(parseInt(selectedAspect.style.left)/100);
		propsBox.style.display = "block";
		
		var newConfigObj;
		var aspKind = selectedAspect.id.substring(0, selectedAspect.id.lastIndexOf("_"));
		
		for (var j=0;j<aspArray[aspKind][1].length;j++) {
		
			document.getElementById("p_label_" + j).innerHTML = configArray[aspKind][j][0];
			
			if (aspArray[aspKind][1][j][1].constructor == Array) {
				newConfigObj = document.createElement("SELECT");
				for (var i=0;i<aspArray[aspKind][1][j][1].length;i++) {
					var oOption = new Option();
					oOption.text = aspArray[aspKind][1][j][1][i];
					oOption.value = aspArray[aspKind][1][j][1][i];
					newConfigObj.options[newConfigObj.options.length] = oOption;
				}
			} else {
				newConfigObj = document.createElement("INPUT");
				newConfigObj.type = "text";
				document.getElementById("td_p_" + j).replaceChild(newConfigObj,document.getElementById("p_" + j));
			}
			newConfigObj.id = "p_" + j;
			newConfigObj.className = "infield";
			document.getElementById("td_p_" + j).replaceChild(newConfigObj,document.getElementById("p_" + j));
			
			document.forms["asp_config_form"].elements[j].name = configArray[aspKind][j][0];
			eval('document.forms["asp_config_form"].elements[j].value = chainArray[n].p' + j);
			document.getElementById("asp_config_row_" + j).style.display = (configArray[aspKind][j][0] == "") ? "none" : rowBodyDisplay;

		}
		
		document.forms["asp_config_form"].xpathEdit.value = chainArray[n].xpath;
		//document.forms["asp_config_form"].displayXML.value = document.getElementById("xmlFromAjax").value;	//== Done on operDrop

	} else {
		propsBox.style.display = "none";
		propsText.innerHTML = "";
	}
}

function removeAspect() {
	var canvas = document.getElementById("canvasDiv");
	if (selectedAspect) {
		canvas.removeChild(selectedAspect);
		for (var n=0;n<chainArray.length;n++) {
			if (chainArray[n].cID == selectedAspect.id) {
				break;
			}
		}
		document.getElementById("exportDiv").removeChild(document.getElementById("export_"+selectedAspect.id));

		var delAsp = chainArray.splice(n,1);
		reArrange(delAsp[0].cX, -1);
	}
	deSelectAspect();
}

function clearCanvas() {
	deSelectAspect();
	var canvas = document.getElementById("canvasDiv");
	canvasChildren = canvas.childNodes;
	while (canvasChildren.length > 0) {
		canvas.removeChild(canvasChildren[0]);
	}
	chainCount = 0;
	chainArray = new Array();
	operationXMLstr = "";

	operIsSet = false;		//- enable adding action on canvas
	if (aRows) {
		for (var k=rowsBegin; k < aRows.length-rowsEnd; k++) {
			aRows[k].onmousedown = startDrag;
		}
	}
	for (var j=0; j < document.forms.length; j++) {
		if (document.forms[j].name != "f1") {
			document.forms[j].reset();
		}
	}

	var exportD = document.getElementById("exportDiv");
	exportChildren = exportD.childNodes;
	while (exportChildren.length > 0) {
		exportD.removeChild(exportChildren[0]);
	}
	document.getElementById("serviceNameSpan").innerHTML = "";
	document.getElementById("portTypeNameSpan").innerHTML = "";
	document.getElementById("operNameSpan").innerHTML = "";
}
//////////////////////////////////////////////////////

//----------


//////////////////////////////////////////////////////
//==	util.js

function snapToGrid(dObj) {
	var oLeft = parseInt(dObj.style.left);
	oLeft = Math.round(oLeft/gridSize)*gridSize + gridStart;
	var oTop = gridVert;
	//var oTop = parseInt(dObj.style.top);
	//oTop = Math.round(oTop/gridSize)*gridSize;
	if (oLeft == chainArray[0].cX) {
		oLeft += gridSize;
	}
	if ((oLeft == chainArray[1].cX) && mAsp) {
		oLeft -= gridSize;
	}
	shiftTo(dObj.style, oLeft, oTop);
}


function sortByX(a, b) {
    var x = a.cX;
    var y = b.cX;
    return ((x < y) ? -1 : ((x > y) ? 1 : 0));
}
//////////////////////////////////////////////////////

function reArrange(posX, arrDir, stX) {
	document.getElementById("connLine").style.width = gridSize*(chainArray.length-1) + "px";
	document.getElementById("connIMG").style.width = gridSize*(chainArray.length-1) + "px";

	if (arrDir > 0) {
		for (i=1;i<chainArray.length-1;i++) {
			if (posX <= chainArray[i].cX) {
				chainArray[i].cX += gridSize;
				document.getElementById(chainArray[i].cID).style.left = chainArray[i].cX + "px";
			}
		}
	} else if (arrDir < 0) {
		for (i=1;i<chainArray.length;i++) {
			if (posX <= chainArray[i].cX) {
				chainArray[i].cX -= gridSize;
				document.getElementById(chainArray[i].cID).style.left = chainArray[i].cX + "px";
			}
		}
	} else {
		shiftTo(mAsp.style, posX, gridVert);
		for (i=0;i<chainArray.length;i++) {
			if ((mAsp.id == chainArray[i].cID)) {
				chainArray[i].cX = posX;
			}
		}
		
		var tempArr1 = chainArray.slice(0,2);
		var tempArr2 = chainArray.slice(2);
		tempArr2.sort(sortByX);
		
		for (i=0;i<tempArr2.length;i++) {
			tempArr2[i].cX = gridStart + gridSize*(i+1);
			document.getElementById(tempArr2[i].cID).style.left = tempArr2[i].cX + "px";
		}
		
		chainArray = tempArr1.concat(tempArr2);
	}
}

//////////////////////////////////////////////////////

function saveConfig() {
	if (selectedAspect) {
		for (var n=0;n<chainArray.length;n++) {
			if (chainArray[n].cID == selectedAspect.id) {
				break;
			}
		}
		var aspKind = selectedAspect.id.substring(0, selectedAspect.id.lastIndexOf("_"));
		chainArray[n].p0 = document.forms["asp_config_form"].elements[0].value;
		chainArray[n].p1 = document.forms["asp_config_form"].elements[1].value;
		chainArray[n].p2 = document.forms["asp_config_form"].elements[2].value;
		chainArray[n].xpath = document.forms["asp_config_form"].xpathEdit.value;
		
		exportConfig(n,aspKind);
	}
}

function exportConfig(n, aspKind, aspID) {
	var expAspectID = (aspID) ? aspID : selectedAspect.id;
	//var configXMLstr = "export_"+expAspectID + "\n";
	var configXMLstr = "";
	configXMLstr += '<?xml version="1.0" encoding="UTF-8"?>\n';
	configXMLstr += '<'+aspArray[aspKind][0][0]+'>\n';
    configXMLstr += '\t<config>\n';
	if (configArray[aspKind][0][0] != "") {
        configXMLstr += '\t\t<property name="'+configArray[aspKind][0][0]+'" value="'+chainArray[n].p0+'" />\n';
	}
	if (configArray[aspKind][1][0] != "") {
        configXMLstr += '\t\t<property name="'+configArray[aspKind][1][0]+'" value="'+chainArray[n].p1+'" />\n';
	}
	if (configArray[aspKind][2][0] != "") {
        configXMLstr += '\t\t<property name="'+configArray[aspKind][2][0]+'" value="'+chainArray[n].p2+'" />\n';
	}
    configXMLstr += '\t</config>\n';
	if (chainArray[n].xpath != "") {
	    configXMLstr += '\t<ruleset name="my'+aspArray[aspKind][0][0]+'">\n';
	        configXMLstr += '\t\t<rule name="myfirstrule">\n';
	            configXMLstr += '\t\t\t<xpath keyExpression="'+chainArray[n].xpath+'"/>\n';        
	        configXMLstr += '\t\t</rule>\n';
	    configXMLstr += '\t</ruleset>\n';
	}
	configXMLstr += '</'+aspArray[aspKind][0][0]+'>';

	document.getElementById("export_"+expAspectID).value = configXMLstr;
	
			document.getElementById("export_"+expAspectID).title = "export_"+expAspectID;		//== FOR DEBUG ONLY
			//alert(configXMLstr);	 //== FOR DEBUG ONLY
}

function saveAspects() {
	if (!operIsSet) {
		alert("Please choose operation first.");
		return;
	}
	var exportAspXMLstr = "";
	for (i=2;i<chainArray.length;i++) {
		var aspKind = chainArray[i].cID.substring(0, chainArray[i].cID.lastIndexOf("_"));
		exportAspXMLstr += '\t\t<advice type="'+aspArray[aspKind][0][0]+'" configurationFile="'+chainArray[i].cID+'.xml" order="'+Math.floor(chainArray[i].cX/100)+'"/>\n';
	}
	document.getElementById("export_operation").value = operationXMLstr + exportAspXMLstr + operationXMLstrEnd;
	
			//alert(document.getElementById("export_operation").value);			//== FOR DEBUG ONLY
			document.forms["export_form"].submit();
}

//==================================================
//-	TEMP -

function showChainTest() {			//== FOR DEBUG ONLY
		//chainArray.sort(sortByX);
	var chainArrayStr = "";
	for (i=0;i<chainArray.length;i++) {
		chainArrayStr += chainArray[i].cID+", "+chainArray[i].cX+", "+chainArray[i].cY+", "+chainArray[i].branch+", "+chainArray[i].p0+", "+chainArray[i].p1+", "+chainArray[i].p2+", "+chainArray[i].xpath+"\n";
	}
	alert(chainArrayStr);
}

//-->
