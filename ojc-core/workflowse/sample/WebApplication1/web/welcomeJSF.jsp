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
<ice:panelGroup style="padding: 5px">
    <br>
    <ice:outputLabel value="Work List Manager"/>
    <br>

</ice:panelGroup>

<!-- Static Tab Set -->
<ice:panelGroup id="tabSetTabs"
                styleClass="exampleBox panelTabSetContainer">
<ice:form>

    <ice:panelTabSet
        selectedIndex="#{staticTabbedPaneExample.focusIndex}"
        tabPlacement="#{staticTabbedPaneExample.tabPlacement}"
        tabChangeListener="#{staticTabbedPaneExample.processTabChange}"
        title="Tab 1">

    <!-- Panel Tab 1 defined with some basic content -->
        <ice:panelTab rendered="#{staticTabbedPaneExample.tabbedPane1Visible}"
                      label="Tab 1"
                      icon="images/menu/recent.gif"
                  title="Tab 1">
        <div style="height:150px;">
            <ice:outputText
                value="{msgs['page.panelTabSet.example.iceFaces.title']}"
                style="font-weight:bold;"/>
            <ice:outputText
                value="{msgs['page.panelTabSet.example.iceFaces.description']}"/>
            <br/>
        </div>
    </ice:panelTab>

    <!-- Panel Tab 2 defined with some basic content -->
        <ice:panelTab rendered="#{staticTabbedPaneExample.tabbedPane2Visible}"
                      label="Tab 2"
                  title="Tab 2">
        <div style="height:150px;">
            <ice:outputText
                value="{msgs['page.panelTabSet.example.iceBrowser.title']}"
                style="font-weight:bold;"/>
            <ice:outputText
                value="{msgs['page.panelTabSet.example.iceBrowser.description']}"/>
            <br/>
        </div>
    </ice:panelTab>

    <!-- Panel Tab 3 defined with some basic content -->
        <ice:panelTab rendered="#{staticTabbedPaneExample.tabbedPane3Visible}"
                      label="Tab 3"
                      icon="images/menu/recent.gif"
                      iconAlignRight="true"
                  title="Tab 3">
        <div style="height:150px;">
            <ice:outputText
                value="{msgs['page.panelTabSet.example.icePdf.title']}"
                style="font-weight:bold;"/>
            <ice:outputText
                value="{msgs['page.panelTabSet.example.icePdf.description']}"/>
        </div>
    </ice:panelTab>
    
    </ice:panelTabSet>
</ice:form>
</ice:panelGroup>

<!-- Panel Layout -->
<ice:panelGroup style="border:1px solid silver; background-color:silver; padding: 5px">
    <ice:panelBorder id="page"
                     style="border:1px solid silver; background-color:yellow; padding: 5px"
                     renderNorth="#{borderLayout.renderNorth}"
                     renderSouth="#{borderLayout.renderSouth}"
                     renderCenter="#{borderLayout.renderCenter}"
                     renderWest="#{borderLayout.renderWest}"
                     renderEast="#{borderLayout.renderEast}">

        <f:facet name="north">
            <ice:outputText id="north"
                            value="North"/>
        </f:facet>

        <f:facet name="west">
            <ice:outputText id="west"
                            value="West"/>
        </f:facet>

        <f:facet name="east">
            <ice:outputText id="east"
                            value="East"/>
        </f:facet>

        <f:facet name="center">
            <ice:outputText id="center"
                            value="Center"/>
        </f:facet>

        <f:facet name="south">
            <ice:outputText id="south"
                            value="South"/>
        </f:facet>
    </ice:panelBorder>
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