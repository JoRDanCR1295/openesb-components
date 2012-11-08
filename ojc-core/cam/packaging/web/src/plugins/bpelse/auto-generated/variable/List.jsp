<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>List Variable</title>
    </head>
    <body>
        <f:view>
            <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
            <h1>Listing Variables</h1>
            <h:form>
                <h:commandLink action="#{variable.createSetup}" value="New Variable"/>
                <br>
                <h:outputText value="Item #{variable.firstItem + 1}..#{variable.lastItem} of #{variable.itemCount}"/>&nbsp;
                <h:commandLink action="#{variable.prev}" value="Previous #{variable.batchSize}" rendered="#{variable.firstItem >= variable.batchSize}"/>&nbsp;
                <h:commandLink action="#{variable.next}" value="Next #{variable.batchSize}" rendered="#{variable.lastItem + variable.batchSize <= variable.itemCount}"/>&nbsp;
                <h:commandLink action="#{variable.next}" value="Remaining #{variable.itemCount - variable.lastItem}"
                               rendered="#{variable.lastItem < variable.itemCount && variable.lastItem + variable.batchSize > variable.itemCount}"/>
                <h:dataTable value='#{variable.variables}' var='item' border="1" cellpadding="2" cellspacing="0">
                    <h:column>
                        <f:facet name="header">
                            <h:outputText value="VariablePK"/>
                        </f:facet>
                        <h:commandLink action="#{variable.detailSetup}" value="#{item.variablePK}"/>
                    </h:column>
                    <h:column>
                        <f:facet name="header">
                            <h:outputText value="Value"/>
                        </f:facet>
                        <h:outputText value="#{item.value}"/>
                    </h:column>
                    <h:column>
                        <f:facet name="header">
                            <h:outputText value="State"/>
                        </f:facet>
                        <h:outputText value="#{item.state}"/>
                    </h:column>
                    <h:column>
                        <h:commandLink value="Destroy" action="#{variable.destroy}">
                            <f:param name="variablePK" value="#{item.variablePK}"/>
                        </h:commandLink>
                        <h:outputText value=" "/>
                        <h:commandLink value="Edit" action="#{variable.editSetup}">
                            <f:param name="variablePK" value="#{item.variablePK}"/>
                        </h:commandLink>
                    </h:column>
                </h:dataTable>
            </h:form>
        </f:view>
    </body>
</html>
