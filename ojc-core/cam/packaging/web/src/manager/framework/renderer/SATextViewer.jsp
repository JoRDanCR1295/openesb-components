<?xml version="1.0" encoding="UTF-8"?>

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
                <webuijsf:head title="Service Assembly Viewer" />
                <webuijsf:frameSet id="ServiceAssemblyFrameSet" rows="10%,90%" >
                     <webuijsf:frame id="saViewerControl" name="saViewerControl" 
                               url="#{SVGRendererBean.serviceAssemblyControlURL}" 
                               frameBorder="true"/>
                     <webuijsf:frame id="saViewerFrame" name="saViewerFrame" 
                               url="#{SVGRendererBean.serviceAssemblyJBITextURL}" 
                               frameBorder="true" 
                               noResize="false"
                               scrolling="true"/>
                 </webuijsf:frameSet>
            </webuijsf:html>
        </webuijsf:page>
    </f:view>

</jsp:root>
