<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
  <jsp:directive.page contentType="text/html" />

<!-- Actions (Top) -->
<webuijsf:button id="deleteselected"
    actionExpression="#{state.deleteSelected}"
    disabled="#{state.deleteSelectedDisabled}"
    onClick="if (confirmDeleteSelectedRows() == false) return false"
    text="#{msgs.bpelse_instances_delete_selected}"
    toolTip="#{msgs.bpelse_instances_delete_selected_tip}"/>
</jsp:root>
