<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>List State</title>
    </head>
    <body>
        <f:view>
            <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
            <h1>Listing States</h1>
            <h:form>
                <h:commandLink action="#{state.createSetup}" value="New State"/>
                <br>
                <h:outputText value="Item #{state.firstItem + 1}..#{state.lastItem} of #{state.itemCount}"/>&nbsp;
                <h:commandLink action="#{state.prev}" value="Previous #{state.batchSize}" rendered="#{state.firstItem >= state.batchSize}"/>&nbsp;
                <h:commandLink action="#{state.next}" value="Next #{state.batchSize}" rendered="#{state.lastItem + state.batchSize <= state.itemCount}"/>&nbsp;
                <h:commandLink action="#{state.next}" value="Remaining #{state.itemCount - state.lastItem}"
                               rendered="#{state.lastItem < state.itemCount && state.lastItem + state.batchSize > state.itemCount}"/>
                <h:dataTable value='#{state.states}' var='item' border="1" cellpadding="2" cellspacing="0">
                    <h:column>
                        <f:facet name="header">
                            <h:outputText value="Id"/>
                        </f:facet>
                        <h:commandLink action="#{state.detailSetup}" value="#{item.id}"/>
                    </h:column>
                    <h:column>
                        <f:facet name="header">
                            <h:outputText value="Bpelid"/>
                        </f:facet>
                        <h:outputText value="#{item.bpelid}"/>
                    </h:column>
                    <h:column>
                        <f:facet name="header">
                            <h:outputText value="Engineid"/>
                        </f:facet>
                        <h:outputText value="#{item.engineid}"/>
                    </h:column>
                    <h:column>
                        <f:facet name="header">
                            <h:outputText value="Status"/>
                        </f:facet>
                        <h:outputText value="#{item.status}"/>
                    </h:column>
                    <h:column>
                        <h:commandLink value="Destroy" action="#{state.destroy}">
                            <f:param name="id" value="#{item.id}"/>
                        </h:commandLink>
                        <h:outputText value=" "/>
                        <h:commandLink value="Edit" action="#{state.editSetup}">
                            <f:param name="id" value="#{item.id}"/>
                        </h:commandLink>
                    </h:column>
                </h:dataTable>
            </h:form>
        </f:view>
    </body>
</html>
