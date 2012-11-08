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
      <webuijsf:head title="#{msgs.svg_viewer_missing}" />
        <webuijsf:body >
          <webuijsf:form id="missingSVGViewer">
            <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
  	    <table border="0" cellspacing="0" cellpadding="0" width="100%">
                <tr> 
                    <td align="left">
                      <webuijsf:alert id="svgViewermsg" type="error"   summary="#{SVGRendererBean.downloadViewerMessage}" />  
                    </td> 
                </tr>
            </table>
         </webuijsf:form>
        </webuijsf:body> 
       </webuijsf:html>
    </webuijsf:page>
    </f:view>
</jsp:root>
