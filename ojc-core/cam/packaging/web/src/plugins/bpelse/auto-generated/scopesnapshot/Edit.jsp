<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>Edit Scopesnapshot</title>
    </head>
    <body>
        <f:view>
            <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
            <h1>Edit scopesnapshot</h1>
            <h:form>
                <h:inputHidden value="#{scopesnapshot.scopesnapshot}" immediate="true"/>
                <h:panelGrid columns="2">
                    <h:outputText value="ScopesnapshotPK:"/>
                    <h:outputText value="#{scopesnapshot.scopesnapshot.scopesnapshotPK}" title="ScopesnapshotPK" />
                    <h:outputText value="Varvalue:"/>
                    <h:inputText id="varvalue" value="#{scopesnapshot.scopesnapshot.varvalue}" title="Varvalue" />
                    <h:outputText value="State:"/>
                    <h:selectOneMenu id="state" value="#{scopesnapshot.scopesnapshot.state}" title="State">
                        <f:selectItems value="#{scopesnapshot.states}"/>
                    </h:selectOneMenu>
                    <h:outputText value="Variable:"/>
                    <h:selectOneMenu id="variable" value="#{scopesnapshot.scopesnapshot.variable}" title="Variable">
                        <f:selectItems value="#{scopesnapshot.variables}"/>
                    </h:selectOneMenu>
                </h:panelGrid>
                <h:commandLink action="#{scopesnapshot.edit}" value="Save"/>
                <br>
                <h:commandLink action="scopesnapshot_list" value="Show All Scopesnapshot"/>
            </h:form>
        </f:view>
    </body>
</html>
