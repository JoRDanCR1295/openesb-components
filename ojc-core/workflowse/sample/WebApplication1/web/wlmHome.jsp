<%-- 
    Document   : wlmHome
    Created on : Feb 3, 2009, 11:06:29 AM
    Author     : mbhasin
--%>

<%@page pageEncoding="UTF-8"%>

<%@taglib prefix="f" uri="http://java.sun.com/jsf/core"%>
<%@taglib prefix="h" uri="http://java.sun.com/jsf/html"%>
<%@taglib prefix="ice" uri="http://www.icesoft.com/icefaces/component"%>

<% 
 	session.invalidate(); 
 %>

<f:view>
    <ice:outputHtml>
        <ice:outputHead>
            <title>WorkList Manager Home Page</title>
            <ice:outputStyle href="./xmlhttp/css/xp/xp.css"/>
        </ice:outputHead>
        <ice:outputBody>
            <ice:form>

                <ice:panelGrid columns="3" style="position:absolute; left:100px; top:100px; padding:5px; width:350px; border:1px solid silver;">
                    <ice:outputLabel for="TxtName" value="User"/>
                    <ice:outputLabel for="TxtName" value="User Type"/>
                    <ice:outputLabel for="TxtName" value="Login"/>

                    <hr><hr><hr>
                    <ice:outputLabel for="TxtName" value="Malkit"/>
                    <ice:outputLabel for="TxtName" value="Wlm Team (Group)(Task 1)"/>
                    <ice:commandLink id="Malkit-wlm_team"
                                     action="userHome"
                                     value="Login"
                                     actionListener="#{buttonsAndLinks.submitButtonListener}"
                                     style="color:blue"/>

                    <ice:outputLabel for="TxtName2" value="Mei"/>
                    <ice:outputLabel for="TxtName" value="Wlm Team (Group)(Task 1)"/>
                    <ice:commandLink id="Mei-wlm_team"
                                     style="color:blue"
                                     action="userHome"
                                     value="Login"
                                     actionListener="#{buttonsAndLinks.submitButtonListener}"/>

                    <ice:outputLabel id = "gabe-escalation" for="TxtName4" value="Gabe"/>
                    <ice:outputLabel for="TxtName" value="Manager (Escalation)(Task 1)"/>
                    <ice:commandLink id="Gabe"
                                     style="color:blue"
                                     action="userHome"
                                     value="Login"
                                     actionListener="#{buttonsAndLinks.submitButtonListener}"/>

                    <ice:outputLabel id = "prashant" for="TxtName6" value="Prashant"/>
                    <ice:outputLabel for="TxtName" value="User (User)(Task 1)"/>
                    <ice:commandLink id="Prashant"
                                     style="color:blue"
                                     action="userHome"
                                     value="Login"
                                     actionListener="#{buttonsAndLinks.submitButtonListener}"/>

                    <ice:outputLabel id = "philip" for="TxtName6" value="Philip"/>
                    <ice:outputLabel for="TxtName" value="User (User)(Task 2)"/>
                    <ice:commandLink id="Philip"
                                     style="color:blue"
                                     action="userHome"
                                     value="Login"
                                     actionListener="#{buttonsAndLinks.submitButtonListener}"/>
                </ice:panelGrid>
            </ice:form>
        </ice:outputBody>
    </ice:outputHtml>
</f:view>
