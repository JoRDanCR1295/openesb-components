<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>Detail of State</title>
    </head>
    <body>
        <f:view>
            <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
            <h1>Detail of state</h1>
            <h:form>
                <h:panelGrid columns="2">
                    <h:outputText value="Id:"/>
                    <h:outputText value="#{state.state.id}" title="Id" />
                    <h:outputText value="Bpelid:"/>
                    <h:outputText value="#{state.state.bpelid}" title="Bpelid" />
                    <h:outputText value="Engineid:"/>
                    <h:outputText value="#{state.state.engineid}" title="Engineid" />
                    <h:outputText value="Status:"/>
                    <h:outputText value="#{state.state.status}" title="Status" />
                </h:panelGrid>
                <h:commandLink action="state_edit" value="Edit" />
                <br>
                <h:commandLink action="state_list" value="Show All State"/>
            </h:form>
        </f:view>
    </body>
</html>
