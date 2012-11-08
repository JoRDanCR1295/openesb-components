<jsp:root version="2.0" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
  <jsp:directive.page contentType="text/html" />
  <f:view>
    <webuijsf:page>
      <webuijsf:html>
          
          
        <webuijsf:head title="Table">
        </webuijsf:head>
        
        <webuijsf:body>
          <webuijsf:form id="form1">
  
            <webuijsf:contentPageTitle title="Table"/>
  
              <!-- Table -->
              <webuijsf:table id="table1"
                paginateButton="true"
                paginationControls="true"
                deselectMultipleButton="true"
                selectMultipleButton="true"
                title="Table Actions">
                    
                <webuijsf:tableRowGroup id="rowGroup1"
                  binding="#{TableBean2.group.tableRowGroup}"
                  selected="#{TableBean2.group.select.selectedState}"
                  sourceData="#{TableBean2.group.data}"
                  sourceVar="data" >
                      
                  <webuijsf:tableColumn id="col0"
                    selectId="select"
                    sort="#{TableBean2.group.select.selectedState}">
                    <webuijsf:checkbox id="select"
                      selected="#{TableBean2.group.select.selected}"
                      selectedValue="#{TableBean2.group.select.selectedValue}"/>
                  </webuijsf:tableColumn>
                      
                  <webuijsf:tableColumn id="col1"    
                    headerText="LastName"
                    rowHeader="true" >
                    <webuijsf:hyperlink text="#{data.value.name}"/>
                  </webuijsf:tableColumn>
                  <webuijsf:tableColumn id="col2"
                    headerText="FirstName" >
                    <webuijsf:staticText text="#{data.value.desc}"/>
                  </webuijsf:tableColumn>
                </webuijsf:tableRowGroup>
  
                <!-- Actions (Top) -->
                <f:facet name="actionsTop">
                  <f:subview id="actionsTop">
                    <jsp:include page="actionsTop2.jsp"/>
                  </f:subview>
                </f:facet>
  
                <!-- Actions (Bottom) -->
                <f:facet name="actionsBottom">
                  <f:subview id="actionsBottom">
                    <jsp:include page="actionsBottom.jsp"/>
                  </f:subview>
                </f:facet>
  
          
              </webuijsf:table>


          </webuijsf:form>
        </webuijsf:body>
      </webuijsf:html>
    </webuijsf:page>
  </f:view>
</jsp:root>
