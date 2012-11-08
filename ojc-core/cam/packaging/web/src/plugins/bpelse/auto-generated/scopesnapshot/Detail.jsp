<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>Detail of Scopesnapshot</title>
    </head>
    <body>
        <f:view>
            <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
            <h1>Detail of scopesnapshot</h1>
            <h:form>
                <h:panelGrid columns="2">
                    <h:outputText value="ScopesnapshotPK:"/>
                    <h:outputText value="#{scopesnapshot.scopesnapshot.scopesnapshotPK}" title="ScopesnapshotPK" />
                    <h:outputText value="Varvalue:"/>
                    <h:outputText value="#{scopesnapshot.scopesnapshot.varvalue}" title="Varvalue" />
                    <h:outputText value="State:"/>
                    <h:outputText value="#{scopesnapshot.scopesnapshot.state}" title="State" />
                    <h:outputText value="Variable:"/>
                    <h:outputText value="#{scopesnapshot.scopesnapshot.variable}" title="Variable" />
                </h:panelGrid>
                <h:commandLink action="scopesnapshot_edit" value="Edit" />
                <br>
                <h:commandLink action="scopesnapshot_list" value="Show All Scopesnapshot"/>
            </h:form>
        </f:view>
    </body>
</html>
