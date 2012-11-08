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
    <ice:panelGrid columns="2" >
        <ice:panelGroup style="padding: 1px">
            <ice:outputLabel style="font-size:16px; " value="Work List Manager"/>
            <br><br>
        </ice:panelGroup>
        <ice:panelGroup style="padding: 1px">
            <ice:form>
                <ice:panelGrid columns="3" style="position:absolute; left:600px; top:10px; font-size:12px;">
                    <ice:outputLabel value="Logged In:"/>
                    <ice:outputLabel style= "font-weight:bold" value="#{buttonsAndLinks.loggedUser} | "/>

                    <ice:commandLink value="Logout"
                                     action="wlmHome"
                                     actionListener="#{buttonsAndLinks.submitButtonListener}"
                                     style="color:blue"/>
                </ice:panelGrid>
            </ice:form>
        </ice:panelGroup>
    </ice:panelGrid>

    <!-- Static Tab Set -->
    <ice:panelGroup id="tabSetTabs">
    <ice:form>

    <ice:panelTabSet
        selectedIndex="#{staticTabbedPaneExample.focusIndex}"
        tabPlacement="#{staticTabbedPaneExample.tabPlacement}"
        tabChangeListener="#{staticTabbedPaneExample.processTabChange}"
        style="width:1480px">

    <ice:panelTab rendered="#{staticTabbedPaneExample.tabbedPane1Visible}"
                  label="Home">
    <div style="height:950px;">

        <ice:panelGrid columns="2" >
            <!-- f:facet name="west" -->
            <ice:panelGroup style="position:absolute; left:15px; top:90px; width:840px; border:1px solid silver; padding: 5px;  height:940px;">


                <ice:panelGrid columns="2" >

                    <ice:panelGroup style="width:700px">
                        <ice:outputText value="Task List"  style="font-weight:bold;"/>
                        <ice:inputText id="SearchTask"
                                       size="45"
                                       maxlength="50"
                                       style="position:absolute; left:150px"
                                       valueChangeListener="#{textFields.effectChangeListener}"
                                       value="#{textFields.name}"
                                       partialSubmit="true"/>
                        <ice:commandButton
                            style="position:absolute; left:462px"
                            value="Search Task"></ice:commandButton>

                        <br><br>
                        <ice:panelGroup style="width: 80%; border:1px solid silver; padding: 5px">
                            <ice:commandButton value="Claim"></ice:commandButton>
                            <ice:commandButton value="Revoke"></ice:commandButton>
                            <ice:commandButton value="Escalate"></ice:commandButton>

                            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                            <ice:selectOneMenu id="SlctCompTyp"
                                               value="#{taskFilterActionsBean.statusSelectedComponent}" 
                                               partialSubmit="true">
                                <f:selectItems id="SlctcompTypeItms"
                                               value="#{taskFilterActionsBean.statusFilterItems}"/>
                            </ice:selectOneMenu>
                            <ice:selectOneMenu id="SlctCompTyp2"
                                               value="#{taskFilterActionsBean.expirationSelectedComponent}"
                                               partialSubmit="true">
                                <f:selectItems id="SlctcompTypeItms2"
                                               value="#{taskFilterActionsBean.expirationFilterItems}"/>
                            </ice:selectOneMenu>
                        </ice:panelGroup>
                    </ice:panelGroup>

                    <ice:panelGroup style="width:200px">
                        <ice:panelGrid columns="1">
                            <ice:outputText style="font-weight:bold; background-color:#E0E0E0" value="Summary"/>
                            <ice:outputText value="Claimed (5)"/>
                            <ice:outputText value="Unclaimed (10)"/>
                            <ice:outputText value="Completed (5)"/>
                        </ice:panelGrid>
                    </ice:panelGroup>

                </ice:panelGrid>

                <br>
                <ice:outputLabel for="SlctCompTyp" value="Select:"/>
                <ice:commandLink action="" style="color:blue">All</ice:commandLink>
                <ice:commandLink style="color:blue">None</ice:commandLink>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                <ice:outputLabel for="SlctCompTyp" value="Assigned To: Me"/>
                <ice:selectBooleanCheckbox id ="userAssigned"
                               valueChangeListener="#{taskFilterActionsBean.effectChangeListener}"
                               value="#{taskFilterActionsBean.userAssigned}"
                               partialSubmit="true"/>

                &nbsp;
                <ice:outputLabel for="SlctCompTyp" value="Group"/>
                <ice:selectBooleanCheckbox id ="groupAssigned"
                               valueChangeListener="#{taskFilterActionsBean.effectChangeListener}"
                               value="#{taskFilterActionsBean.groupAssigned}"
                               partialSubmit="true"/>

                <ice:dataTable value="#{newTasksBean.taskList}" var="task">
                    <ice:column>
                        <ice:outputLabel for="SlctUsr"/>
                        <ice:selectBooleanCheckbox id="SlctUsr"
                                                   partialSubmit="true"/>
                    </ice:column>

                    <ice:column id="taskInstanceId">
                        <ice:rowSelector id="selected"
                                         value="#{task.selected}"
                                         multiple="true"/>
                        <f:facet name="header">
                            <ice:outputText id="column1" value="Task Id"/>
                        </f:facet>
                        <ice:outputText value="#{task.taskInstanceId}"/>
                    </ice:column>
                    <ice:column id="title">
                        <f:facet name="header">
                            <ice:outputText id="column2" value="Title"/>
                        </f:facet>
                        <ice:outputText value="#{task.title}"/>
                    </ice:column>
                    <ice:column id="priority">
                        <f:facet name="header">
                            <ice:outputText id="column3" value="Priority"/>
                        </f:facet>
                        <ice:outputText value="#{task.priority}"/>
                    </ice:column>
                    <ice:column id="escalationDate">
                        <f:facet name="header">
                            <ice:outputText id="column4" value="Escalation Date"/>
                        </f:facet>
                        <ice:outputText value="#{task.escalationDate}"/>
                    </ice:column>
                    <ice:column id="escalationDutation">
                        <f:facet name="header">
                            <ice:outputText id="column5" value="Escalation Duration"/>
                        </f:facet>
                        <ice:outputText value="#{task.escalationDutation}"/>
                    </ice:column>
                    <ice:column id="createdDate">
                        <f:facet name="header">
                            <ice:outputText id="column6" value="Created Date"/>
                        </f:facet>
                        <ice:outputText value="#{task.createdDate}"/>
                    </ice:column>
                    <ice:column id="Status">
                        <f:facet name="header">
                            <ice:outputText id="column7" value="Status"/>
                        </f:facet>
                        <ice:outputText value="#{task.status}"/>
                    </ice:column>
                </ice:dataTable>
            </ice:panelGroup>

            <ice:panelGroup  style="position:absolute; left:870px; top:90px; width:600px; border:1px solid silver; padding: 5px; height:940px;">
                <ice:outputText value="Selected Task"  style="font-weight:bold;"/>
                <br><br>

                <ice:panelGroup style="text-align:right; width: 50%; border:1px solid silver; padding: 5px">
                    <ice:commandButton value="Escalate"></ice:commandButton>
                    <ice:commandButton style="align:right" value="Reassign"></ice:commandButton>
                </ice:panelGroup>

                <ice:panelGrid columns="2" style="padding:5px; background-color:#E0E0E0; width:50%">
                    <ice:outputLabel for="TxtName"
                                     value="Order Id"/>
                    <ice:inputText id="TxtName1"
                                   size="25"
                                   maxlength="25"
                                   valueChangeListener="#{textFields.effectChangeListener}"
                                   value="#{textFields.name}"
                                   partialSubmit="true"/>
                    <ice:outputLabel for="TxtName2"
                                     value="Purchaser Name"/>
                    <ice:inputText id="TxtName3"
                                   size="25"
                                   maxlength="25"
                                   valueChangeListener="#{textFields.effectChangeListener}"
                                   value="#{textFields.name}"
                                   partialSubmit="true"/>
                    <ice:outputLabel for="TxtName4"
                                     value="Product Id"/>
                    <ice:inputText id="TxtName5"
                                   size="25"
                                   maxlength="25"
                                   valueChangeListener="#{textFields.effectChangeListener}"
                                   value="#{textFields.name}"
                                   partialSubmit="true"/>
                    <ice:outputLabel for="TxtName6"
                                     value="Amount"/>
                    <ice:inputText id="TxtName7"
                                   size="25"
                                   maxlength="25"
                                   valueChangeListener="#{textFields.effectChangeListener}"
                                   value="#{textFields.name}"
                                   partialSubmit="true"/>
                </ice:panelGrid>

                <ice:panelGroup style="text-align:right; width: 50%; border:1px solid silver; padding: 5px">
                    <ice:commandButton value="Save"></ice:commandButton>
                    <ice:commandButton value="Submit"></ice:commandButton>
                </ice:panelGroup>

            </ice:panelGroup>
        </ice:panelGrid>
        <br/>
    </div>
    </ice:panelTab>

    <ice:panelTab rendered="#{staticTabbedPaneExample.tabbedPane2Visible}"
                  label="Completed">
        <div style="height:150px;">
            <ice:outputText
                value="Completed Tasks" style="font-weight:bold;"/>
            <br/>
        </div>
    </ice:panelTab>

    <ice:panelTab rendered="#{staticTabbedPaneExample.tabbedPane3Visible}"
                  label="Configuration" iconAlignRight="true">
        <div style="height:150px;">
            <ice:outputText
                value="Configuration" style="font-weight:bold;"/>
        </div>
    </ice:panelTab>

    </ice:panelTabSet>

    </ice:form>

</ice:panelGroup>
</ice:panelGroup>


</ice:outputBody>
</ice:outputHtml>

</f:view> 