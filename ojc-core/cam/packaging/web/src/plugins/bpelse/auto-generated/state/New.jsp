<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>New State</title>
    </head>
    <body>
        <f:view>
            <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
            <h1>New state</h1>
            <h:form>
                <h:panelGrid columns="2">
                    <h:outputText value="Id:"/>
                    <h:inputText id="id" value="#{state.state.id}" title="Id" />
                    <h:outputText value="Bpelid:"/>
                    <h:inputText id="bpelid" value="#{state.state.bpelid}" title="Bpelid" />
                    <h:outputText value="Engineid:"/>
                    <h:inputText id="engineid" value="#{state.state.engineid}" title="Engineid" />
                    <h:outputText value="Status:"/>
                    <h:inputText id="status" value="#{state.state.status}" title="Status" />
                </h:panelGrid>
                <h:commandLink action="#{state.create}" value="Create"/>
                <br>
                <h:commandLink action="state_list" value="Show All State"/>
            </h:form>
        </f:view>
    </body>
</html>
