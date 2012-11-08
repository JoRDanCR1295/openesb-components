<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>List Engine</title>
    </head>
    <body>
        <f:view>
            <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
            <h1>Listing Engines</h1>
            <h:form>
                <h:commandLink action="#{engine.createSetup}" value="New Engine"/>
                <br>
                <h:outputText value="Item #{engine.firstItem + 1}..#{engine.lastItem} of #{engine.itemCount}"/>&nbsp;
                <h:commandLink action="#{engine.prev}" value="Previous #{engine.batchSize}" rendered="#{engine.firstItem >= engine.batchSize}"/>&nbsp;
                <h:commandLink action="#{engine.next}" value="Next #{engine.batchSize}" rendered="#{engine.lastItem + engine.batchSize <= engine.itemCount}"/>&nbsp;
                <h:commandLink action="#{engine.next}" value="Remaining #{engine.itemCount - engine.lastItem}"
                               rendered="#{engine.lastItem < engine.itemCount && engine.lastItem + engine.batchSize > engine.itemCount}"/>
                <h:dataTable value='#{engine.engines}' var='item' border="1" cellpadding="2" cellspacing="0">
                    <h:column>
                        <f:facet name="header">
                            <h:outputText value="Id"/>
                        </f:facet>
                        <h:commandLink action="#{engine.detailSetup}" value="#{item.id}"/>
                    </h:column>
                    <h:column>
                        <f:facet name="header">
                            <h:outputText value="Location"/>
                        </f:facet>
                        <h:outputText value="#{item.location}"/>
                    </h:column>
                    <h:column>
                        <f:facet name="header">
                            <h:outputText value="Expiration"/>
                        </f:facet>
                        <h:outputText value="#{item.expiration}"/>
                    </h:column>
                    <h:column>
                        <h:commandLink value="Destroy" action="#{engine.destroy}">
                            <f:param name="id" value="#{item.id}"/>
                        </h:commandLink>
                        <h:outputText value=" "/>
                        <h:commandLink value="Edit" action="#{engine.editSetup}">
                            <f:param name="id" value="#{item.id}"/>
                        </h:commandLink>
                    </h:column>
                </h:dataTable>
            </h:form>
        </f:view>
    </body>
</html>
