<!--
var rowArr;
var celArr = new Array();
var tableArray = new Array();
var chainCount = 0;
var chainArray = new Array();

function tableInfoObj(targNS,serviceName,portName,portType,operName) {
    this.targNS = targNS;
    this.serviceName = serviceName;
    this.portName = portName;
    this.portType = portType;
    this.operName = operName;
}

function chainObj(cID,cX,cY,branch,p0,p1,p2,xpath) {
    this.cID = cID;
    this.cX = cX;
    this.cY = cY;
    this.branch = branch;
    this.p0 = p0;
    this.p1 = p1;
    this.p2 = p2;
    this.xpath = xpath;
}

//=======	Add configuration for each aspect
//var aspInitArray = new Array("ca","ta","ra","qa","la","mta");
var aspInitArray = new Array("ca","ta","ra","qa","la","mta","cbr","t");

var aspArray = new Array();
var aspArrayCount = "asp" + aspArray.length +"_" + aspInitArray[aspArray.length++];

aspArray[aspArrayCount] = [ [ "caching", "Caching Advice" ], [ [ "", "" ],  [ "", "" ],  [ "", "" ] ] ];
aspArrayCount = "asp" + aspArray.length +"_" + aspInitArray[aspArray.length++];
aspArray[aspArrayCount] = [ [ "throttling", "Throttling Advice" ], [ [ "rate", "10" ],  [ "queue-length", "1000" ],  [ "", "" ] ] ];
aspArrayCount = "asp" + aspArray.length +"_" + aspInitArray[aspArray.length++];
aspArray[aspArrayCount] = [ [ "retry", "Auto-Reconnect Advice" ], [ [ "rate", "10" ],  [ "timeout", "1000" ],  [ "", "" ] ] ];
aspArrayCount = "asp" + aspArray.length +"_" + aspInitArray[aspArray.length++];
aspArray[aspArrayCount] = [ [ "queuing", "Queuing Advice" ], [ [ "queue-length", "1000" ],  [ "timeout", "1000" ],  [ "", "" ] ] ];
aspArrayCount = "asp" + aspArray.length +"_" + aspInitArray[aspArray.length++];
aspArray[aspArrayCount] = [ [ "logging", "Logging Advice" ], [ [ "level", ["SEVERE","WARNING","INFO","CONFIG","FINE","FINER","FINEST"] ],  [ "rotation-policy", ["DAILY","WEEKLY","MONTHLY","YEARLY"] ],  [ "log-file", "/tmp/loggingse.log" ] ] ];
aspArrayCount = "asp" + aspArray.length +"_" + aspInitArray[aspArray.length++];
aspArray[aspArrayCount] = [ [ "message-tracking", "Message Tracking Advice" ], [ [ "mode", ["database", "file"] ],  [ "", "" ],  [ "", "" ] ] ];
aspArrayCount = "asp" + aspArray.length +"_" + aspInitArray[aspArray.length++];
aspArray[aspArrayCount] = [ [ "contentBasedRouting", "Content-Based Routing Advice" ], [ [ "", "" ],  [ "", "" ],  [ "", "" ] ] ];
aspArrayCount = "asp" + aspArray.length +"_" + aspInitArray[aspArray.length++];
aspArray[aspArrayCount] = [ [ "tee", "T-Advice" ], [ [ "", "" ],  [ "", "" ],  [ "", "" ] ] ];
	//aspArray[i][0][0]		//=	aspN_XX XML node name
	//aspArray[i][0][1]		//=	aspN_XX display name
	//aspArray[i][1][0][0]	//=	aspN_XX prop label
	//aspArray[i][1][0][1]		//=	aspN_XX list of props for prop name aspArray[i][1][0][0]

var configArray = new Array();

configArray["asp0_ca"] = [ [ "", "" ],  [ "", "" ],  [ "", "" ] ];
configArray["asp1_ta"] = [ [ "rate", "10" ],  [ "queue-length", "1000" ],  [ "", "" ] ];
configArray["asp2_ra"] = [ [ "rate", "10" ],  [ "timeout", "1000" ],  [ "", "" ] ];
configArray["asp3_qa"] = [ [ "queue-length", "1000" ],  [ "timeout", "1000" ],  [ "", "" ] ];
configArray["asp4_la"] = [ [ "level", "INFO" ],  [ "rotation-policy", "DAILY" ],  [ "log-file", "/tmp/loggingse.log" ] ];
configArray["asp5_mta"] = [ [ "mode", "database" ],  [ "", "" ],  [ "", "" ] ];
//configArray["asp5_mta"] = [ [ "mode", ["database", "file"] ],  [ "", "" ],  [ "", "" ] ];
configArray["asp6_cbr"] = [ [ "", "" ],  [ "", "" ],  [ "", "" ] ];
configArray["asp7_t"] = [ [ "", "" ],  [ "", "" ],  [ "", "" ] ];
	//configArray["asp1_ta"][0][0]		//=	p0 name (label)
	//configArray["asp1_ta"][0][1]		//=	p0 value

var aspIMGstr = "";
for (var j=0;j<aspInitArray.length;j++) {
	var aspKind = "asp" + j +"_" + aspInitArray[j];
	aspIMGstr += '<img src="images/'+aspInitArray[j]+'.gif" width="30" height="34" border="0" id="'+aspKind+'" title="'+aspArray[aspKind][0][1]+'"> ';
}

function initImgDrag() {
	for (var j=0;j<aspInitArray.length;j++) {
		document.getElementById("asp" + j +"_" + aspInitArray[j]).onmousedown = startDragAspect;
	}
}

//--------------------------------------------

var operationXMLstr = "";
var operationXMLstrEnd = '\t</aspect>\n</aspectmap>\n';

//-->
