<%@page contentType="text/html"%>
<%@page pageEncoding="UTF-8"%>

<%@taglib prefix="f" uri="http://java.sun.com/jsf/core"%>
<%@taglib prefix="h" uri="http://java.sun.com/jsf/html"%>
<%@taglib prefix="ice" uri="http://www.icesoft.com/icefaces/component"%>

<f:view>
<ice:outputHtml>
<ice:outputHead>
    <title>JSP Page</title>
    <ice:outputStyle href="./xmlhttp/css/xp/xp.css"/>
</ice:outputHead>
<ice:outputBody>


<ice:panelGroup>
    <ice:panelGroup style="border:1px solid silver; padding: 5px">
        <br>
        <ice:outputLabel value="Work List Manager"/>
        <br>
        
    </ice:panelGroup>

    <ice:panelDivider
        dividerPosition="#{panelDividerBean.position}"
        orientation="#{panelDividerBean.orientation}"
        style="width: 100%; height: 750px;">




    <f:facet name="first">
        <ice:panelGroup style="padding: 10px;">
            <ice:outputText value="New Tasks" style="line-height:2em"/>

            <ice:form style="width: 80%;">
                <ice:panelGroup style="border:1px solid silver; padding: 5px">
                    <ice:commandButton value="Claim"></ice:commandButton>
                    <ice:commandButton value="Escalate"></ice:commandButton>
                    <!-- Select one menu - component type -->

                    <ice:selectOneMenu id="SlctCompTyp"
                                       value="#{newTasksActions.selectedComponent}"
                                       partialSubmit="true">
                        <f:selectItems id="SlctcompTypeItms"
                                       value="#{newTasksActions.componentItems}"/>
                    </ice:selectOneMenu>
                    <br><br>

                    <ice:outputLabel for="SlctCompTyp" value="Select:"/>
                    <ice:commandLink action="" style="color:blue">All</ice:commandLink>
                    <ice:commandLink style="color:blue">None</ice:commandLink>
                </ice:panelGroup>
            </ice:form>

            <ice:form style="width: 80%;">
                <ice:dataTable value="#{employeeBean.employees}" var="employee">
                    <ice:column id="First Name">
                        <ice:rowSelector id="selected"
                                         value="#{employee.selected}"
                                         multiple="true"/>
                        <f:facet name="header">
                            <ice:outputText id="column1"
                                            value="First Name"/>
                        </f:facet>
                        <ice:outputText value="#{employee.firstName}"/>
                    </ice:column>
                    <ice:column id="Last Name">
                        <f:facet name="header">
                            <ice:outputText id="column2"
                                            value="Last Name"/>
                        </f:facet>
                        <ice:outputText value="#{employee.lastName}"/>
                    </ice:column>
                </ice:dataTable>
            </ice:form>

        </ice:panelGroup>
    </f:facet>

    <f:facet name="second">
        <ice:panelGroup style="padding: 10px;">
            <ice:outputText value="My Task List" style="line-height:2em"/>

            <ice:form style="width: 50%;">
                <ice:panelGroup style="border:1px solid silver; padding: 5px">
                    <ice:commandButton value="Claim"></ice:commandButton>
                    <ice:commandButton value="Revoke"></ice:commandButton>
                    <ice:commandButton value="Escalate"></ice:commandButton>
                    <ice:outputText value="#{color}" />
                    <!-- Select one menu - component type -->
                    <ice:outputLabel for="SlctCompTyp"/>
                    <ice:selectOneMenu id="SlctCompTyp"
                                       value="#{myTasksActions.selectedComponent}"
                                       partialSubmit="true">
                        <f:selectItems id="SlctcompTypeItms"
                                       value="#{myTasksActions.componentItems}"/>
                    </ice:selectOneMenu>

                    <br><br>

                    <ice:outputLabel for="SlctCompTyp" value="Select:"/>
                    <ice:commandLink action="" style="color:blue">All</ice:commandLink>
                    <ice:commandLink style="color:blue">None</ice:commandLink>
                </ice:panelGroup>
            </ice:form>

            <ice:form style="width: 45%;">
                <ice:dataTable value="#{employeeBean.employees}" var="employee">
                    <ice:column id="First Name">
                        <ice:rowSelector id="selected"
                                         value="#{employee.selected}"
                                         multiple="true"/>
                        <f:facet name="header">
                            <ice:outputText id="column1"
                                            value="First Name"/>
                        </f:facet>
                        <ice:outputText value="#{employee.firstName}"/>
                    </ice:column>
                    <ice:column id="Last Name">
                        <f:facet name="header">
                            <ice:outputText id="column2"
                                            value="Last Name"/>
                        </f:facet>
                        <ice:outputText value="#{employee.lastName}"/>
                    </ice:column>
                </ice:dataTable>
            </ice:form>

        </ice:panelGroup>
    </f:facet>
    </ice:panelDivider>

</ice:panelGroup>
</ice:outputBody>
</ice:outputHtml>

</f:view> 