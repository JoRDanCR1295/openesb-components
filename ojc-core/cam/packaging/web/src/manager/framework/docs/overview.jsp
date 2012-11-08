<jsp:root version="2.0" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html"
          xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
    <jsp:directive.page contentType="text/html"/>
    <f:view>    
        <webuijsf:page>
            <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
            
            <webuijsf:html id="html">  
                <webuijsf:head id="head" title="Common Tasks">                        
                    <webuijsf:link rel="shortcut icon" url="/images/favicon.ico" type="image/x-icon" />
                </webuijsf:head>
                
                <webuijsf:body id="body">
                    
                    <webuijsf:contentPageTitle title="#{msgs.commontasks_overviewTitle}" />
                    
                    <table style="font-size:10pt;margin-left:10px;margin-rignt:10px">
                        <tr>
                            <td>
                                <f:verbatim><![CDATA[&nbsp;&nbsp;&nbsp;]]></f:verbatim>
                            </td>
                        </tr>
                        <tr>
                            <td>
                                <p><webuijsf:helpInline id="pageHelp1" text="#{msgs.docs_overview1}" /></p>
                                <p><webuijsf:helpInline id="pageHelp2" text="#{msgs.docs_overview2}" /></p>
                                <p><webuijsf:helpInline id="pageHelp3" text="#{msgs.docs_overview3}" /></p>
                            </td>
                        </tr>
                    </table>
                    
                </webuijsf:body>
                
            </webuijsf:html>  
        </webuijsf:page>
    </f:view>
</jsp:root>
