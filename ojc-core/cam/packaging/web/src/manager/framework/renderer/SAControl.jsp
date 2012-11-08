<jsp:root version="1.2" 
          xmlns:f="http://java.sun.com/jsf/core" 
          xmlns:h="http://java.sun.com/jsf/html" 
          xmlns:jsp="http://java.sun.com/JSP/Page" 
          xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
                 

    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" 
                        pageEncoding="UTF-8"/>
    <f:view>
    <webuijsf:page frame="true">
       <webuijsf:html>
      <webuijsf:head title="#{msgs.svg_SAViewer_control}" />
     <jsp:scriptlet>
       String serviceAssemblyName = (String)session.getAttribute("appName");
       String queryParamName = com.sun.jbi.cam.common.GenericConstants.COMPONENT_PNAME;
       String isVisible = 
        session.getAttribute("legendVisible")=="true" ? "true" : "false";
    </jsp:scriptlet>
    <script   LANGUAGE="JavaScript"  type="text/javascript">
             var legendFlag = <jsp:expression>isVisible</jsp:expression>;      
             var pName = "<jsp:expression>serviceAssemblyName</jsp:expression>"; 
             var paramName = "<jsp:expression>queryParamName</jsp:expression>"; 
             var rootElement =getSVGRootElement();
             var lastResponseValue;
            
            function getSVGRootElement() {
               var svgFrame = parent.saViewerFrame;
               var root;
               if(!isFF15Browser()) { 
                try {
                 var svgObj = svgFrame.document.embeds[0].window;
                 root = svgObj.document.rootElement;
                 } catch(e) {
                   // prevent ie browser error pop if svg frame not available 
                 }
                 
               } else {
                  root = svgFrame.document.rootElement;
               }
               return root;
            }
            
           function showLegend() {
                var legend = null;
                var legendBox = null;
                if (parent.saViewerFrame) {
                   if(rootElement=="undefined") {
                     rootElement = getSVGRootElement();
                   }
                   if(!isFF15Browser()) { 
                      legend = rootElement.getElementById('legend');
                      legendBox = rootElement.getElementById('legendbox');
                   } else {
                        legend = getSVGObjectInFF(rootElement,"legend");
                        legendBox = getSVGObjectInFF(legend,"legendbox");
                   }
                   if (legendFlag) {
                        legend.setAttributeNS(null, 'visibility', 'hidden');
                    } else {
                        legend.setAttributeNS(null, 'visibility', 'visible');
                   }
                }
            };
            
            function isIE7Browser(){
	      var isIE7 = false;
	      try {  
	            isIE7 = ((navigator.userAgent).indexOf('MSIE 7.0') != -1);
                   } catch(e) {
	      } 
                 return isIE7;
            };

            function isFF15Browser() {
             var isFF15 = false;
             if(window.XMLHttpRequest &amp;&amp; !isIE7Browser()) {
                  isFF15 =true;
             }
              return isFF15;
            };
            
            
            
            function getSVGObjectInFF(parentElement,objectId) {
               var count = parentElement.childNodes.length; 
               for(var index=0 ; index &lt; count; index++) {
                  if(parentElement.childNodes[index].id == objectId) {
                    return parentElement.childNodes[index];
                  }
               }
               return null;
            };

        
            function processServiceUnitsStateRequest() { 
              var url = "/cam/faces/serviceUnitsState.jsp?"+paramName+"=" +pName;
              if(window.XMLHttpRequest &amp;&amp; !isIE7Browser()) { 
                   // firefox 1.5.x
         	   xmlhttpobject = new XMLHttpRequest();
        	   xmlhttpobject.onreadystatechange = getServiceUnitsState;
        	   try{
	               if (netscape.security.PrivilegeManager.enablePrivilege) { 
        		     netscape.security.PrivilegeManager.enablePrivilege("UniversalBrowserRead");
        	       }
        	    } catch (e) {
                    }
        	    try{ 
        	       xmlhttpobject.open("GET", url, true);
                    } catch (e) {
                             alert(e);
                    }
                   xmlhttpobject.send(null);
        	} else  { // Ie
                   xmlhttpobject = new ActiveXObject("Microsoft.XMLHTTP");
                    if (xmlhttpobject) {
                        xmlhttpobject.onreadystatechange = getServiceUnitsState;
                        xmlhttpobject.open("GET", url, true);
                        xmlhttpobject.send();
                    }
                } 
            };
            
            
            function getServiceUnitsState() { 
              if(window.XMLHttpRequest) { // Non-IE browsers
                 try{
                  if (netscape.security.PrivilegeManager.enablePrivilege) {
                     netscape.security.PrivilegeManager.enablePrivilege("UniversalBrowserRead");
                   }
               } catch (e) {
               }
            }
            if(xmlhttpobject.readyState == 4) {
               if(xmlhttpobject.status == 200) {
                  var serverResponse = xmlhttpobject.responseText
                  var serviceUnitsStateArray = parseResponse(serverResponse);
                  if(serverResponse != lastResponseValue) {
                    // only update if there server response does not match
                    // last server response value
                    updateFrames(serviceUnitsStateArray);
                    lastResponseValue = serverResponse;
                  }
                  setTimeout('getnextStateSample()',3000);
               } else {
                  alert('There was a problem with the request.');
               }
            }
         }; 

         
    	 function parseResponse(responseText) {
           var splitResponseData = responseText.split(";");
           return splitResponseData;
           
         };

         function getnextStateSample() {
            processServiceUnitsStateRequest();
         }
         
         function  updateFrames(serviceUnitsStateArray) {
              var startedCount = 0;
              var stoppedCount = 0;
              var shutdownCount = 0;
              var suFillColor;
              var count = serviceUnitsStateArray.length;
              for(var index =0 ; index &lt; count ; index++) {
                 var nameValuepair = serviceUnitsStateArray[index];
                 var nameValuearray = nameValuepair.split("=");
                 var suName = nameValuearray[0];
                 var suState = nameValuearray[1];
                 switch (suState) {
                   case "1": 
                     startedCount = startedCount +1;
                    // white for started su
                    suFillColor = "#FFFFFF";
                     break;
                   case "2":  
                     stoppedCount = stoppedCount +1;
                     // light gray for stopped su
                     suFillColor = "#CCCCCC";
                    break;
                   case "3":  
                     shutdownCount = shutdownCount +1;
                     // dark gray for shutdown su
                     suFillColor = "#A0A0A0";
                     break;
                 }
                 updateSVGFrame(suName,suState,suFillColor);
              }
              updateControlButtons(count,startedCount,stoppedCount,
                                                  shutdownCount);
         }

         function updateControlButtons(count,startedCount,stoppedCount,
                                                        shutdownCount) {
             
             if(startedCount == count) {
                // all started
                document.getElementById("sacontrolform:saStop").disabled=false;      
                document.getElementById("sacontrolform:saStop").className= "Btn2";
                document.getElementById("sacontrolform:saStart").disabled=true;      
                document.getElementById("sacontrolform:saStart").className= "Btn2Dis";
                document.getElementById("sacontrolform:saShutdown").disabled=true;      
                document.getElementById("sacontrolform:saShutdown").className= "Btn2Dis";
             } else if(stoppedCount == count) {
                // all stopped
                document.getElementById("sacontrolform:saStop").disabled=true;      
                document.getElementById("sacontrolform:saStop").className= "Btn2Dis";
                document.getElementById("sacontrolform:saStart").disabled=false;      
                document.getElementById("sacontrolform:saStart").className= "Btn2";
                document.getElementById("sacontrolform:saShutdown").disabled=false;      
                document.getElementById("sacontrolform:saShutdown").className= "Btn2";
              } else if(shutdownCount == count) {
                // all shutdown
                document.getElementById("sacontrolform:saStart").disabled=false;      
                document.getElementById("sacontrolform:saStart").className= "Btn2";
                document.getElementById("sacontrolform:saStop").disabled=true;      
                document.getElementById("sacontrolform:saStop").className= "Btn2Dis";
                document.getElementById("sacontrolform:saShutdown").disabled=true;      
                document.getElementById("sacontrolform:saShutdown").className= "Btn2Dis";
              } else if(startedCount > 0 &amp;&amp; stoppedCount > 0) {
               // one or more started or stopped
                document.getElementById("sacontrolform:saStart").disabled=false;      
                document.getElementById("sacontrolform:saStart").className= "Btn2";
                document.getElementById("sacontrolform:saStop").disabled=false;      
                document.getElementById("sacontrolform:saStop").className= "Btn2";
                document.getElementById("sacontrolform:saShutdown").disabled=false;      
                document.getElementById("sacontrolform:saShutdown").className= "Btn2";
              }  else if(startedCount > 0 &amp;&amp; shutdownCount > 0) {
               // one or more started or shutdown
                document.getElementById("sacontrolform:saStart").disabled=false;      
                document.getElementById("sacontrolform:saStart").className= "Btn2";
                document.getElementById("sacontrolform:saStop").disabled=false;      
                document.getElementById("sacontrolform:saStop").className= "Btn2";
                document.getElementById("sacontrolform:saShutdown").disabled=true;      
                document.getElementById("sacontrolform:saShutdown").className= "Btn2Dis";
              }  else if(stoppedCount > 0 &amp;&amp; shutdownCount > 0) {
               // one or more stopped or shutdown
                document.getElementById("sacontrolform:saStart").disabled=false;      
                document.getElementById("sacontrolform:saStart").className= "Btn2";
                document.getElementById("sacontrolform:saStop").disabled=true;      
                document.getElementById("sacontrolform:saStop").className= "Btn2Dis";
                document.getElementById("sacontrolform:saShutdown").disabled=false;      
                document.getElementById("sacontrolform:saShutdown").className= "Btn2";
              } 

              
         } 
         
         function updateSVGFrame(suName, suState,suFillColor) {
            var suObject;
            // if this frame load prior to the svg frame rootelement
            // will be undefined
            if(rootElement== null) {
              rootElement = getSVGRootElement();
            }
             if(rootElement== null) {
              // svg frame not available skip this update cycle 
               return;
             }
            if(!isFF15Browser()) {
              
              suObject = rootElement.getElementById(suName);
            } else {
              var suContainerElement = getSVGObjectInFF(rootElement,
                                                 "serviceUnitContainer");
              suObject = getSVGObjectInFF(suContainerElement,suName);
            }
             
            suObject.setAttributeNS(null, 'fill', suFillColor);
            
         }
        </script>
         <!-- webuijsf:body onLoad="processServiceUnitsStateRequest()" -->
         <webuijsf:body onLoad="processServiceUnitsStateRequest()">
          <webuijsf:form id="sacontrolform" target="_parent">
            <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
  	    <table border="0" cellspacing="0" cellpadding="0" width="100%">
                <tr> 
                 <th  width="50%" align="left" valign="middle">
                    <webuijsf:button disabled="#{SAControlBean.SAStartButtonDisabled}" id="saStart" text="#{msgs.control_startAction}" actionExpression="#{SAControlBean.start}"/>
                    <webuijsf:button disabled="#{SAControlBean.SAStopButtonDisabled}" id="saStop" text="#{msgs.control_stopAction}" actionExpression="#{SAControlBean.stop}"/>
                    <webuijsf:button disabled="#{SAControlBean.SAShutdownButtonDisabled}" id="saShutdown" text="#{msgs.control_shutdownAction}" actionExpression="#{SAControlBean.shutdown}"/>
                    <webuijsf:hiddenField disabled="#{SAControlBean.SAQueryFlagCleared}"/>
                 </th>   
                        <jsp:scriptlet>
                            // Get the SAControlBean instnace from FacesContext
                            javax.faces.context.FacesContext context = 
                            javax.faces.context.FacesContext.getCurrentInstance();
                            
                            javax.faces.el.ValueBinding bpVisualValueBinding = 
                            context.getApplication().createValueBinding(com.sun.jbi.cam.manager.framework.renderers.svg.SAControlBean.BEAN_NAME);
                            
                            com.sun.jbi.cam.manager.framework.renderers.svg.SAControlBean saControlBean = 
                            ( com.sun.jbi.cam.manager.framework.renderers.svg.SAControlBean) bpVisualValueBinding.getValue(context);        
                            boolean isTextView = saControlBean.isTextView();
                            if(!isTextView) { 
                        </jsp:scriptlet>
	   	 <th width="50%" align="right" valign="middle">
                     <webuijsf:button id="legendBtn" text="#{SAControlBean.legendControlString}" 
                           actionExpression="#{SAControlBean.switchLegendVisibility}" onClick="showLegend();return false"/>
                 </th>   
               <jsp:scriptlet>
                   }
               </jsp:scriptlet>
                </tr>
                <tr>
                    <td>
                      <webuijsf:label  text="#{SAControlBean.assemblyStatus}" />  
                    </td> 
                </tr>
            </table>
            <table cellspacing="0" cellpadding="0" >
                <tr>
                  <td>
                    <webuijsf:button id="switchSVGView" text="#{SAControlBean.viewActionText}" actionExpression="#{SAControlBean.switchView}"/>
                  </td> 
                </tr>
                <tr>
                    <td>
                      <webuijsf:label   text="#{SAControlBean.inlinehelp}" />  
                    </td> 
                </tr>
            </table>
         </webuijsf:form>
        </webuijsf:body> 
       </webuijsf:html>
    </webuijsf:page>
    </f:view>
</jsp:root>
