<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>Detail of Engine</title>
    </head>
    <body>
        <f:view>
            <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
            <h1>Detail of engine</h1>
            <h:form>
                <h:panelGrid columns="2">
                    <h:outputText value="Id:"/>
                    <h:outputText value="#{engine.engine.id}" title="Id" />
                    <h:outputText value="Location:"/>
                    <h:outputText value="#{engine.engine.location}" title="Location" />
                    <h:outputText value="Expiration:"/>
                    <h:outputText value="#{engine.engine.expiration}" title="Expiration" />
                </h:panelGrid>
                <h:commandLink action="engine_edit" value="Edit" />
                <br>
                <h:commandLink action="engine_list" value="Show All Engine"/>
            </h:form>
        </f:view>
    </body>
</html>
