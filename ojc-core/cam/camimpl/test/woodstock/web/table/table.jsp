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
                title="Table Actions">
                    
                <webuijsf:tableRowGroup id="rowGroup1"
                  sourceData="#{TableBean.names}"
                  sourceVar="name">
                      
                  <webuijsf:tableColumn id="col1"    
                    headerText="LastName"
                    rowHeader="true" >
                    <webuijsf:staticText text="#{name.value.lastName}"/>
                  </webuijsf:tableColumn>
                  <webuijsf:tableColumn id="col2"
                    headerText="FirstName" >
                    <webuijsf:staticText text="#{name.value.firstName}"/>
                  </webuijsf:tableColumn>
                </webuijsf:tableRowGroup>
  
                <!-- Actions (Top) -->
                <f:facet name="actionsTop">
                  <f:subview id="actionsTop">
                    <jsp:include page="actionsTop.jsp"/>
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
