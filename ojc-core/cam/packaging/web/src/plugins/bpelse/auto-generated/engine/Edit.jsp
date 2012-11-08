<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>Edit Engine</title>
    </head>
    <body>
        <f:view>
            <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
            <h1>Edit engine</h1>
            <h:form>
                <h:inputHidden value="#{engine.engine}" immediate="true"/>
                <h:panelGrid columns="2">
                    <h:outputText value="Id:"/>
                    <h:outputText value="#{engine.engine.id}" title="Id" />
                    <h:outputText value="Location:"/>
                    <h:inputText id="location" value="#{engine.engine.location}" title="Location" />
                    <h:outputText value="Expiration:"/>
                    <h:inputText id="expiration" value="#{engine.engine.expiration}" title="Expiration" />
                </h:panelGrid>
                <h:commandLink action="#{engine.edit}" value="Save"/>
                <br>
                <h:commandLink action="engine_list" value="Show All Engine"/>
            </h:form>
        </f:view>
    </body>
</html>
