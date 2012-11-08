<?xml version="1.0" encoding="UTF-8"?>

<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">

    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
    
    <f:view>
        <webuijsf:page frame="true">
            <webuijsf:html>
                <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
                <webuijsf:head title="#{msgs.plugins_managePlugins}" />

                 <table>

                  <tr style="height:5px"><td></td></tr>
                
                  <tr><td>
                  <webuijsf:staticText id="title" text="#{msgs.plugins_title}" style="margin-left:10px;font-weight:bold" /> 
                  </td></tr>

                  <tr> 
                    <td ><hr color="#E39333" size="1" style="margin-left:10px"/></td>
                  </tr>                 

                  <tr style="height:5px"><td></td></tr>
                  <tr style="height:10px"><td></td></tr>

                  <tr><td>
                  <webuijsf:upload id="upload" style="margin-left:10px"
                            uploadedFile = "#{PluginsManagerBean.uploadedFile}"
                            required="true"
                            label="#{msgs.plugins_uploadFile}"
                            toolTip="#{msgs.plugins_uploadFileTooltip}"
                             >
                  </webuijsf:upload>
                  </td></tr>
                  <tr style="height:20px"><td></td></tr>

                  <!-- File Uploader Button -->
                  <tr><td>
                  <webuijsf:button primary="true" text="#{msgs.plugins_deploy}" id="deploy" style="margin-left:10px"
                           actionExpression="#{PluginsManagerBean.fileName}" >
                   </webuijsf:button>
                  </td></tr>
                         
                 </table>



            </webuijsf:html>
        </webuijsf:page>
    </f:view>

</jsp:root>



