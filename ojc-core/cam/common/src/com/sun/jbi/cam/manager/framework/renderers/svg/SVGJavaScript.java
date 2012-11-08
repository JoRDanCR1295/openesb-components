/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)SVGJavaScript.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.renderers.svg;

public class SVGJavaScript {

    // the following is the javascript added to the svg that allow
    // the viewer to display tooltip with information and change the color
    // when the mouse hover over the svg element
    private static final String SCRIPT_HEADER = 

        "	 <script type=\"text/ecmascript\"><![CDATA[ \n" +
        "	     var SVGDocument = null; \n" +
        "	     var SVGRoot = null;  \n" +
        "	     var SVGViewBox = null; \n" +
        "	     var svgns = 'http://www.w3.org/2000/svg'; \n" +
        "	     var xlinkns = 'http://www.w3.org/1999/xlink'; \n" +
        "	     var toolTip = null; \n" +
        "	     var TrueCoords = null; \n" +
        "	     var tipBox = null; \n" +
        "	     var tipText = null; \n" +
        "	     var tipTitle0 = null; \n" +
        "	     var tipTitle = null; \n" +
        "	     var tipTitle2 = null; \n" +
        "	     var tipDesc = null; \n" +
        "	     var lastElement = null; \n" +
        "	     var titleText = ''; \n" +
        "	     var titleDesc = ''; \n" +
        "	     var statisticToolTip = null;\n" +
        "	     var statisticTipbox = null; \n" +
        "	     var statisticTipText =  null;\n" +
        "	     var statisticTipTitle0 = null; \n" +
        "	     var statisticTipTitle1 = null; \n" +
        "	     var statisticTipTitle2 = null; \n" +
        "	     var statisticTipTitle3 = null; \n" +
        "	     var statisticTipTitle4 = null; \n" +
        "	     var statisticTipTitle5 = null; \n" +
        "	     var statisticTipTitle6 = null; \n" +
        "	     var statisticTipTitle7 = null; \n" +
        "	     var statisticTipDesc = null; \n" +
        "    	     var tipScale  = null; \n" +
        "	     var latestHref = \"\"; \n " +
        "            var frameHeight = null; \n" +
        "            var frameWidth = null; \n\n" +
        "            var lastSUSelectedID = null;  \n\n";
    
     private static final String SCRIPT_INIT_FUNCTION = 
    
        "	 function init(evt) { \n" +
        "	     SVGDocument = evt.target.ownerDocument; \n" +
        "	     SVGRoot = SVGDocument.documentElement; \n" +
        "	     TrueCoords = SVGRoot.createSVGPoint(); \n" +
        "	     toolTip = SVGDocument.getElementById('ToolTip'); \n" +
        "	     statisticToolTip = SVGDocument.getElementById('statisticToolTip'); \n" +
        "	     tipBox = SVGDocument.getElementById('tipbox'); \n" +
        "	     tipText = SVGDocument.getElementById('tipText'); \n" +
        "	     tipTitle0 = SVGDocument.getElementById('tipTitle0'); \n" +
        "	     tipTitle = SVGDocument.getElementById('tipTitle'); \n" +
        "	     tipTitle2 = SVGDocument.getElementById('tipTitle2'); \n" +
        "	     tipDesc = SVGDocument.getElementById('tipDesc'); \n" +
        "	     statisticTipTitle0 = SVGDocument.getElementById('statisticTipTitle0'); \n" +
        "	     statisticTipTitle1 = SVGDocument.getElementById('statisticTipTitle1'); \n" +
        "	     statisticTipTitle2 = SVGDocument.getElementById('statisticTipTitle2'); \n" +
        "	     statisticTipTitle3 = SVGDocument.getElementById('statisticTipTitle3'); \n" +
        "	     statisticTipTitle4 = SVGDocument.getElementById('statisticTipTitle4'); \n" +
        "	     statisticTipTitle5 = SVGDocument.getElementById('statisticTipTitle5'); \n" +
        "	     statisticTipTitle6 = SVGDocument.getElementById('statisticTipTitle6'); \n" +
        "	     statisticTipTitle7 = SVGDocument.getElementById('statisticTipTitle7'); \n" +
        "	     statisticTipDesc = SVGDocument.getElementById('statisticTipDesc'); \n" +
        "	     statisticTipText =  SVGDocument.getElementById('statisticTipText');\n" +
        "	     statisticTipbox =  SVGDocument.getElementById('statisticTipbox');\n" +
        "	     SVGRoot.addEventListener('mouseover', ShowTooltip, false); \n" +
        "	     SVGRoot.addEventListener('mouseout', HideTooltip, false); \n" +
        "	     SVGRoot.addEventListener('mousedown', displayServiceUnit, false); \n" +
        "            moveLegend(); \n" +   
        "	 }; \n\n";

     
    private static final String SCRIPT_GETTRUECOORDS_FUNCTION = 
    
        "	 function GetTrueCoords(evt) {\n" +
        "	     var newScale = SVGRoot.currentScale; \n" +
        "	     var translation = SVGRoot.currentTranslate; \n" +
        "	     TrueCoords.x = (evt.clientX - translation.x)/newScale; \n" +
        "	     TrueCoords.y = (evt.clientY - translation.y)/newScale; \n" +
        "	 }; \n\n";


    private static final String SCRIPT_HIDETOOLTIP_FUNCTION = 
            
        "	 function HideTooltip( evt ) {\n" +
        "	     var typeValue = ''; \n" +
        "	     var targetElement = evt.target; \n" +
        "	     var typeValue = targetElement.tagName; \n" +
        "	     var idValue = targetElement.getAttributeNS(null, 'id'); \n" +
        "            var hrefValue = targetElement.getAttributeNS(null, 'HREF'); \n" +
        "	     if(hrefValue.length > 0) {\n" +
        "	        statisticToolTip.setAttributeNS(null, 'visibility', 'hidden');\n" +
        "	        latestHref = \"\";\n" +
        "	        if(!window.XMLHttpRequest || isIE7Browser()) { // IE browsers\n" +
        "                  parent.lastURL = \"\"; \n" +      
        "	        }\n" +
        "	     }else{\n" +
        "	        toolTip.setAttributeNS(null, 'visibility', 'hidden'); \n" +
        "	     }\n" +
        "	     mouseOutSVGElement(idValue,typeValue,hrefValue);  \n" +
        "	 }; \n\n";

     private static final String SCRIPT_SHOW_TOOLTIP_FUNCTION = 
            
        "	 function ShowTooltip( evt ) {\n" +
        "	     GetTrueCoords( evt ); \n" +
        "	     var tipScale = 1/SVGRoot.currentScale; \n" +
        "	     var textWidth = 0; \n" +
        "	     var tspanWidth = 0; \n" +
        "	     var boxHeight = 20; \n" +
        "	     tipBox.setAttributeNS(null, 'transform', 'scale(' + tipScale + ',' + tipScale + ')' ); \n" +
        "	     tipText.setAttributeNS(null, 'transform', 'scale(' + tipScale + ',' + tipScale + ')' ); \n" +
        "	     statisticTipbox.setAttributeNS(null, 'transform', 'scale(' + tipScale + ',' + tipScale + ')' ); \n" +
        "	     statisticTipText.setAttributeNS(null, 'transform', 'scale(' + tipScale + ',' + tipScale + ')' ); \n" +
        "	     var titleValue = ''; \n" +
        "	     var descValue = ''; \n" +
        "	     var typeValue = ''; \n" +
        "	     var targetElement = evt.target; \n" +
        "	     if(lastElement != targetElement ) \n" +
        "	     { \n" +
        "	        typeValue = targetElement.tagName; \n" +
        "	        idValue = targetElement.getAttributeNS(null, 'id');   \n" +
        "	        // use xmlhttp to get statistics for any element \n" +
        "	        // that provides HREF attribute \n" +
        "	        hrefValue = targetElement.getAttributeNS(null, 'HREF');\n" +
        " 		if(hrefValue.length > 0) {\n" +
        "	           mouseOverSVGElement(idValue,typeValue,hrefValue);  \n\n" +
        "	 	   processStatistics(hrefValue);\n" +
        "		   return ;	\n" +
        "	        }\n" +
        "	        mouseOverSVGElement(idValue,typeValue,'');  \n\n" +
        "               var targetTitle = targetElement.getElementsByTagName('title').item(0); \n" +
        "	        if(targetTitle ) \n" +
        "	        { \n" +
        "	           titleValue = targetTitle.firstChild.nodeValue; \n" +
        "	        } \n" +
        "	        var targetDesc = targetElement.getElementsByTagName('desc').item(0); \n" +
        "	        if(targetDesc ) \n" +
        "	        { \n" +
        "	           descValue = targetDesc.firstChild.nodeValue; \n" +
        "	           if('' == titleValue ) \n" +
        "	           { \n" +
        "	              titleValue = descValue; \n" +
        "	              descValue = ''; \n" +
        "	           } \n" +
        "	        } \n" +
        "	        if('' == titleValue ) \n" +
        "	        { \n" +
        "	           titleValue = targetElement.getAttributeNS(null, 'id'); \n" +
        "	        } \n" +
        "	        var titleDisplay = 'none'; \n" +
        "	        if('' != titleValue ) {\n" +
        "	           if(typeValue==\"polyline\") {\n" +
	    " 		          var splitPoint = (titleValue.indexOf(\" ===> \") != -1) ? \" ===> \" : \" <=== \"; \n" +
	    "		          titleValueN = titleValue.split(splitPoint); \n" +
	    "		          tipTitle0.firstChild.nodeValue = titleValueN[0]; \n" +
	    "         	      tipTitle.firstChild.nodeValue = splitPoint; \n" +
	    "		          tipTitle2.firstChild.nodeValue = titleValueN[1]; \n" +
	    "		          tipTitle.setAttributeNS(null, 'dx', 10); \n" +
	    "                 tipTitle.setAttributeNS(null, 'dy', 12); \n" +
	    "		          tipTitle2.setAttributeNS(null, 'dy', 12); \n" +
	    "		          tipBox.setAttributeNS(null, 'height', 48); \n" +
	    "              } else { \n" +
	    "		          tipTitle0.firstChild.nodeValue = \"\"; \n" +
	    "   		      tipTitle.firstChild.nodeValue = titleValue; \n" +
	    "		          tipTitle2.firstChild.nodeValue = \"\"; \n" +
	    "		          tipTitle.setAttributeNS(null, 'dx', 0); \n" +
	    "		          tipTitle0.setAttributeNS(null, 'dy', 0); \n" +
	    "		          tipTitle.setAttributeNS(null, 'dy', 0); \n" +
	    "		          tipTitle2.setAttributeNS(null, 'dy', 0); \n" +
	    "		          tipBox.setAttributeNS(null, 'height', 20); \n" +
        "	           }\n" +
        "	           titleDisplay = 'inline'; \n" +
        "	        }\n" +
        "	        tipTitle.setAttributeNS(null, 'display', titleDisplay ); \n" +
        "	        var descDisplay = 'none'; \n" +
        "	        if('' != descValue ) \n" +
        "	        { \n" +
        "	           tipDesc.firstChild.nodeValue = descValue; \n" +
        "	           descDisplay = 'inline'; \n" +
        "	        } \n" +
        "	        tipDesc.setAttributeNS(null, 'display', descDisplay ); \n" +
        "	     } \n" +
        "	     if('' != titleValue ) \n" +
        "	     { \n" +
        "	        var xPos = TrueCoords.x + (10 * tipScale); \n" +
        "	        var yPos = TrueCoords.y + (10 * tipScale); \n" +
        "	        var outline = tipText.getBBox(); \n" +
        "	        tipBox.setAttributeNS(null, 'width', Number(outline.width) + 10);\n" +
        "		    if (typeValue==\"polyline\") { \n" +
	    "  		       tipBox.setAttributeNS(null, 'height', 48); \n" +
	    "		    } else { \n" +
	    "  		       tipBox.setAttributeNS(null, 'height', Number(outline.height) + 10); \n" +
	    "		    } \n" +
        "	        toolTip.setAttributeNS(null, 'transform', 'translate(' + xPos + ',' + yPos + ')'); \n" +
        "	        toolTip.setAttributeNS(null, 'visibility', 'visible');\n" +
        "	      } \n" +
        "        }; \n\n"; 

    private static final String SCRIPT_MOUSE_FUNCTIONS = 
        
        "	 function mouseOverSVGElement (id,type,hrefvalue) { \n" +
        "	     if(type==\"polyline\") {\n" +
	"  	        SVGDocument.getElementById(id).setAttribute(\"class\", 'actPath'); \n" +
  	"  	        setLabelColor(SVGDocument.getElementById(id).getAttributeNS(null, 'stroke')); \n" +
        "	     } else if (type ==\"circle\") {\n" +
        "	        SVGDocument.getElementById(id).setAttribute('r','4'); \n" +
        "	     } else if (type ==\"path\") {\n" +
        "	        SVGDocument.getElementById(id).setAttribute('stroke-opacity','1.0'); \n" +
        "	        SVGDocument.getElementById(id).setAttribute('stroke-width','3'); \n" +
        "	     } else if (type ==\"rect\" && (hrefValue.length > 0)) {\n" +
        "	        SVGDocument.getElementById(id).setAttribute('stroke-opacity','1.0'); \n" +
        "               if(id == lastSUSelectedID) {\n" +    
        "	           SVGDocument.getElementById(id).setAttribute('stroke-width','3'); \n" +
        "	        } else { \n" +
        "	           SVGDocument.getElementById(id).setAttribute('stroke-width','2'); \n" +
        "	        }\n" +
        "	     }\n" +
        "	 };\n" +

        "	 function mouseOutSVGElement (id,type,hrefValue) { \n" +
        "	     if(type ==\"path\" || type==\"polyline\") {\n" +
        "//	        SVGDocument.getElementById(id).setAttribute('stroke-opacity','0.5'); \n" +
        "//	        SVGDocument.getElementById(id).setAttribute('stroke-width','1'); \n" +
	"  	        SVGDocument.getElementById(id).setAttribute(\"class\", 'inactPath');\n" +
        "	     } else  if (type ==\"circle\") {\n" +
        "	        SVGDocument.getElementById(id).setAttribute('r','2'); \n" +
        "	     } else  if (type ==\"rect\" && (hrefValue.length > 0)) {\n" +
        "	        SVGDocument.getElementById(id).setAttribute('stroke-opacity','0.5'); \n" +
        "               if(id == lastSUSelectedID) {\n" +    
        "	           SVGDocument.getElementById(id).setAttribute('stroke-width','3'); \n" +
        "	        } else { \n" +
        "	           SVGDocument.getElementById(id).setAttribute('stroke-width','1'); \n" +
        "	        }\n" +
         "	     }\n" +
	"            setLabelColor(\"\"); \n" +
        "        };\n\n" ;
        
    
   private static final String  SET_LABEL_COLOR = 
        "        function setLabelColor(clr) { \n" +
	"            tipTitle0.setAttributeNS(null, 'fill', clr); \n" +
	"            tipTitle.setAttributeNS(null, 'fill', clr); \n" +
	"            tipTitle2.setAttributeNS(null, 'fill', clr); \n" +
        "        }\n\n";

   private static final String SERVICE_UNIT_DISPLAY_REDIRECT =
           
        "	 function displayServiceUnit(evt) { \n" +
        "	    var targetElement = evt.target; \n" +
        "	    hrefValue = targetElement.getAttributeNS(null, 'HREF');\n" +
        "	    var typeValue = targetElement.tagName; \n" +
        "	    // only process svg rectangle objects that have HREF attribute \n" +
        "	    if((hrefValue.length==0)) { \n" +
        "	        return; \n" +
        "	    } \n" +
 	"           var idValue = targetElement.getAttributeNS(null, 'id'); \n" +
        "           SVGDocument.getElementById(idValue).setAttribute('stroke','#ff0000'); \n" +
	"           SVGDocument.getElementById(idValue).setAttribute('stroke-opacity','1.0'); \n" +
	"           SVGDocument.getElementById(idValue).setAttribute('stroke-width','3'); \n\n" +
        " 	    if(lastSUSelectedID != null && idValue != lastSUSelectedID) { \n" +
	"               SVGDocument.getElementById(lastSUSelectedID).setAttribute('stroke-opacity','0.5'); \n" +
	"               SVGDocument.getElementById(lastSUSelectedID).setAttribute('stroke-width','1'); \n" +
	"               SVGDocument.getElementById(lastSUSelectedID).setAttribute('stroke','#000000'); \n" +
	"           } \n\n" +
        "           lastSUSelectedID = idValue; \n\n" +
        "	    if(lastElement != targetElement )   \n" +
        "	    { \n" +
        "	       if(window.XMLHttpRequest && !isIE7Browser()) { // Non-IE browsers\n" +
        "	          parent.document.getElementById(\"serviceUnitFrame\").src = hrefValue; \n" +
        "	       } else { \n" +
        "	          parent.parent.document.getElementById(\"serviceUnitFrame\").src = hrefValue; \n" +
        "	       }; \n" +
        "           }; \n" +
        "        }; \n\n" ; 
         

    private static final String PROCESS_STATISTICS =
       
        "	 function processStatistics(href) { \n" +
        "	    if(href==\"\") {\n" +
        "	       return;\n" +
        "	    }	\n" +
        "	    var params =  href.substring(href.indexOf(\"?\"));\n" +
        "	    var url = \"/cam/faces/stats.jsp\"+params;\n" +
        "	    latestHref = url;\n" +
        "	    if(window.XMLHttpRequest && !isIE7Browser()) { // Non-IE browsers\n" +
        "	       xmlhttpobject = new XMLHttpRequest();\n" +
        "	       xmlhttpobject.onreadystatechange = displayStatisticContents;\n" +
        "	       try{\n" +
	"                  if (netscape.security.PrivilegeManager.enablePrivilege) { \n" +
        "		     netscape.security.PrivilegeManager.enablePrivilege(\"UniversalBrowserRead\");\n" +
        "	           }\n" +
        "	       } catch (e) {\n" +
        "	       }\n" +
        "	       try{\n" +
        "		  xmlhttpobject.open(\"GET\", url, true);\n" +
        "	       } catch (e) {\n" +
        "		 alert(e);\n" +
        "	       }\n" +
        "	       xmlhttpobject.send(null);\n" +
        "	    } else  { // IE\n" +
        "	       parent.retrieveStatisticContentsIE(url); \n " +
        "	    } \n" +
        "	 }; \n\n";

    
  private static final String NEXT_STATS_SAMPLE = 
       
        "	 function getnextstatisticSample() { \n" +
        "	    processStatistics(latestHref);\n" +
        "	 };\n\n";
        
  private static final String DISPLAY_STATS_CONTENT = 

        "	 function displayStatisticContents() { \n" +
        "	    if(latestHref==\"\") {\n" +
        "	       return;\n" +
        "	    }	\n" +
        "	    if(window.XMLHttpRequest) { // Non-IE browsers\n" +
        "	       try{\n" +
	"                  if (netscape.security.PrivilegeManager.enablePrivilege) { \n" +
        "		     netscape.security.PrivilegeManager.enablePrivilege(\"UniversalBrowserRead\");\n" +
        "	           }\n" +
        "	       } catch (e) {\n" +
        "	       }\n" +
        "	    }\n" +
        "	    if(xmlhttpobject.readyState == 4) {\n" +
        "	       if(xmlhttpobject.status == 200) {\n" +
        "	          parseResponse(xmlhttpobject.responseText);\n" +
        "	          positionStatisticTextBox();\n" +
        "	          setTimeout('getnextstatisticSample()',1000);\n" +
        "	       } else {\n" +
        "	          alert('There was a problem with the request.');\n" +
        "	       }\n" +
        "	    }\n" +
        "	 }; \n\n";
                
 
  private static final String POSITION_STATS_TEXTBOX = 

        "	function positionStatisticTextBox() {\n" +
        "	   var xPos = TrueCoords.x + (10 * tipScale); \n" +
        "	   var yPos = TrueCoords.y + (10 * tipScale); \n" +
        "	   var outline = statisticTipText.getBBox(); \n" +
        "	   statisticTipbox.setAttributeNS(null, 'width', Number(outline.width) + 10);\n" +
        "	   statisticTipbox.setAttributeNS(null, 'height', Number(outline.height) + 10); \n" +
        "	   statisticToolTip.setAttributeNS(null, 'transform', 'translate(' + xPos + ',' + yPos + ')'); \n" +
        "	   statisticToolTip.setAttributeNS(null, 'visibility', 'visible');\n" +
        "	}\n\n";
  
   private static final String PARSE_RESPONSE = 

        "	 function parseResponse(responseText) { \n" +
        "	    var splitResponseData = responseText.split(\";\");\n" +
        " /*           if(splitResponseData.length==8) { \n" +
        "              statisticTipDesc.firstChild.nodeValue = \"\"; \n" +
        "	       statisticTipTitle0.firstChild.nodeValue = splitResponseData[0]; \n" +
        "	       statisticTipTitle1.firstChild.nodeValue = splitResponseData[1]; \n" +
        "	       statisticTipTitle2.firstChild.nodeValue = splitResponseData[2]; \n" +
        "	       statisticTipTitle3.firstChild.nodeValue = splitResponseData[3]; \n" +
        "	       statisticTipTitle4.firstChild.nodeValue = splitResponseData[4]; \n" +
        "	       statisticTipTitle5.firstChild.nodeValue = splitResponseData[5]; \n" +
        "	       statisticTipTitle6.firstChild.nodeValue = splitResponseData[6]; \n" +
        "	       statisticTipTitle7.firstChild.nodeValue = splitResponseData[7]; \n" +
        "           } else {   */\n" + 
        "              statisticTipDesc.firstChild.nodeValue = splitResponseData[0]; \n" +
        "	       statisticTipTitle0.firstChild.nodeValue = splitResponseData[1]; \n" +
        "	       statisticTipTitle1.firstChild.nodeValue = splitResponseData[2]; \n" +
        "	       statisticTipTitle2.firstChild.nodeValue = splitResponseData[3]; \n" +
        "	       statisticTipTitle3.firstChild.nodeValue = splitResponseData[4]; \n" +
        "	       statisticTipTitle4.firstChild.nodeValue = splitResponseData[5]; \n" +
        "	       statisticTipTitle5.firstChild.nodeValue = splitResponseData[6]; \n" +
        "	       statisticTipTitle6.firstChild.nodeValue = splitResponseData[7]; \n" +
        "	       statisticTipTitle7.firstChild.nodeValue = splitResponseData[8]; \n" +
        "//	    }\n" +
        "	 };\n\n";
           
   private static final String MOVE_LEGEND = 
           
        "	 function moveLegend(){\n" +
        "	     if(window.XMLHttpRequest && !isIE7Browser()) {\n" +
        "	         // this frame dimension in firefox\n" +
        "	        frameHeight= parent.window.frames['saViewerFrame'].innerHeight;\n" +
        "	        frameWidth= parent.window.frames['saViewerFrame'].innerWidth;\n" +
        "	     }else{\n" +
        "	        // this frame dimension in IE \n" +
        "	        frameHeight= (parent.document.body.clientHeight)\n" +
        "	        frameWidth= (parent.document.body.clientWidth)\n" +
        "	     }\n" +
        "            // move the legend to its final position \n " +
        "            legendgroup =  SVGDocument.getElementById('legend'); \n" +
        "            var tx = 'translate('+(frameWidth - 190)+',' + 10 +')';\n"+   
        "	     legendgroup.setAttributeNS(null, 'transform', tx);\n" +
        "	 }\n\n";
  
  private static final String IS_IE7_BROWSER = 
 
        "	 function isIE7Browser(){\n" +
	"            var isIE7 = false; \n" +
	"            try {  \n" +
	"               isIE7 = (parent.navigator.userAgent).indexOf(\"MSIE 7.0\") != -1; \n" +
	"            } catch(e) { \n" +
	"            } \n" +
        "            return isIE7;  \n" +   
        "	 }\n\n";        
 
                
   private static final String SCRIPT_FOOTER = 
        "    ]]></script>  \n" + 
        "   <rect x='0' y='0' width='100%' height='100%' fill='white'/>\n\n";
    
   private static final String STYLE = 
        "    <style type=\"text/css\"><![CDATA[ \n" + 
        "    .inactPath {stroke-opacity:0.5; stroke-width:1;} \n" + 
        "    .actPath {stroke-opacity:1.0; stroke-width:3;} \n" + 
        "    ]]></style> \n" ;
    
   protected static final String SCRIPT_BLOCK = 
                  SCRIPT_HEADER + 
                  SCRIPT_INIT_FUNCTION +
                  SCRIPT_GETTRUECOORDS_FUNCTION + 
                  SCRIPT_HIDETOOLTIP_FUNCTION + 
                  SCRIPT_SHOW_TOOLTIP_FUNCTION + 
                  SCRIPT_MOUSE_FUNCTIONS + 
                  SET_LABEL_COLOR + 
                  SERVICE_UNIT_DISPLAY_REDIRECT +
                  PROCESS_STATISTICS +
                  NEXT_STATS_SAMPLE +
                  DISPLAY_STATS_CONTENT +
                  POSITION_STATS_TEXTBOX + 
                  PARSE_RESPONSE +
                  MOVE_LEGEND + 
                  IS_IE7_BROWSER +
                  SCRIPT_FOOTER + 
                  STYLE;
    
}
