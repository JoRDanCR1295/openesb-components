<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>List Scopesnapshot</title>
    </head>
    <body>
        <f:view>
            <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
            <h1>Listing Scopesnapshots</h1>
            <h:form>
                <h:commandLink action="#{scopesnapshot.createSetup}" value="New Scopesnapshot"/>
                <br>
                <h:outputText value="Item #{scopesnapshot.firstItem + 1}..#{scopesnapshot.lastItem} of #{scopesnapshot.itemCount}"/>&nbsp;
                <h:commandLink action="#{scopesnapshot.prev}" value="Previous #{scopesnapshot.batchSize}" rendered="#{scopesnapshot.firstItem >= scopesnapshot.batchSize}"/>&nbsp;
                <h:commandLink action="#{scopesnapshot.next}" value="Next #{scopesnapshot.batchSize}" rendered="#{scopesnapshot.lastItem + scopesnapshot.batchSize <= scopesnapshot.itemCount}"/>&nbsp;
                <h:commandLink action="#{scopesnapshot.next}" value="Remaining #{scopesnapshot.itemCount - scopesnapshot.lastItem}"
                               rendered="#{scopesnapshot.lastItem < scopesnapshot.itemCount && scopesnapshot.lastItem + scopesnapshot.batchSize > scopesnapshot.itemCount}"/>
                <h:dataTable value='#{scopesnapshot.scopesnapshots}' var='item' border="1" cellpadding="2" cellspacing="0">
                    <h:column>
                        <f:facet name="header">
                            <h:outputText value="ScopesnapshotPK"/>
                        </f:facet>
                        <h:commandLink action="#{scopesnapshot.detailSetup}" value="#{item.scopesnapshotPK}"/>
                    </h:column>
                    <h:column>
                        <f:facet name="header">
                            <h:outputText value="Varvalue"/>
                        </f:facet>
                        <h:outputText value="#{item.varvalue}"/>
                    </h:column>
                    <h:column>
                        <f:facet name="header">
                            <h:outputText value="State"/>
                        </f:facet>
                        <h:outputText value="#{item.state}"/>
                    </h:column>
                    <h:column>
                        <f:facet name="header">
                            <h:outputText value="Variable"/>
                        </f:facet>
                        <h:outputText value="#{item.variable}"/>
                    </h:column>
                    <h:column>
                        <h:commandLink value="Destroy" action="#{scopesnapshot.destroy}">
                            <f:param name="scopesnapshotPK" value="#{item.scopesnapshotPK}"/>
                        </h:commandLink>
                        <h:outputText value=" "/>
                        <h:commandLink value="Edit" action="#{scopesnapshot.editSetup}">
                            <f:param name="scopesnapshotPK" value="#{item.scopesnapshotPK}"/>
                        </h:commandLink>
                    </h:column>
                </h:dataTable>
            </h:form>
        </f:view>
    </body>
</html>
