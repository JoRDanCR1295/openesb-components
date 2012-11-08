<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>New Scopesnapshot</title>
    </head>
    <body>
        <f:view>
            <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
            <h1>New scopesnapshot</h1>
            <h:form>
                <h:panelGrid columns="2">
                    <h:outputText value="ScopesnapshotPK:"/>
                    <h:inputText id="scopesnapshotPK" value="#{scopesnapshot.scopesnapshot.scopesnapshotPK}" title="ScopesnapshotPK" />
                    <h:outputText value="Varvalue:"/>
                    <h:inputText id="varvalue" value="#{scopesnapshot.scopesnapshot.varvalue}" title="Varvalue" />
                    <h:outputText value="State:" rendered="#{scopesnapshot.scopesnapshot.state == null}"/>
                    <h:selectOneMenu id="state" value="#{scopesnapshot.scopesnapshot.state}" title="State" rendered="#{scopesnapshot.scopesnapshot.state == null}">
                        <f:selectItems value="#{scopesnapshot.states}"/>
                    </h:selectOneMenu>
                    <h:outputText value="Variable:" rendered="#{scopesnapshot.scopesnapshot.variable == null}"/>
                    <h:selectOneMenu id="variable" value="#{scopesnapshot.scopesnapshot.variable}" title="Variable" rendered="#{scopesnapshot.scopesnapshot.variable == null}">
                        <f:selectItems value="#{scopesnapshot.variables}"/>
                    </h:selectOneMenu>
                </h:panelGrid>
                <h:commandLink action="#{scopesnapshot.create}" value="Create"/>
                <br>
                <h:commandLink action="scopesnapshot_list" value="Show All Scopesnapshot"/>
            </h:form>
        </f:view>
    </body>
</html>
