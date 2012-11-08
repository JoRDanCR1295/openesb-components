<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>
<%@taglib uri="http://java.sun.com/jsf/core" prefix="f" %>
<%@taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<html>
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>Edit Variable</title>
    </head>
    <body>
        <f:view>
            <h:messages errorStyle="color: red" infoStyle="color: green" layout="table"/>
            <h1>Edit variable</h1>
            <h:form>
                <h:inputHidden value="#{variable.variable}" immediate="true"/>
                <h:panelGrid columns="2">
                    <h:outputText value="VariablePK:"/>
                    <h:outputText value="#{variable.variable.variablePK}" title="VariablePK" />
                    <h:outputText value="Value:"/>
                    <h:inputText id="value" value="#{variable.variable.value}" title="Value" />
                    <h:outputText value="State:"/>
                    <h:selectOneMenu id="state" value="#{variable.variable.state}" title="State">
                        <f:selectItems value="#{variable.states}"/>
                    </h:selectOneMenu>
                </h:panelGrid>
                <h:commandLink action="#{variable.edit}" value="Save"/>
                <br>
                <h:commandLink action="variable_list" value="Show All Variable"/>
            </h:form>
        </f:view>
    </body>
</html>
