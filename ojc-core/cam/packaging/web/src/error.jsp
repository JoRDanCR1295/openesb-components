<jsp:root version="2.0" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">
    <jsp:directive.page contentType="text/html" />
    <f:view>
        <webuijsf:page>
            <webuijsf:html>
                <f:loadBundle basename="com.sun.jbi.cam.common.resources.Bundle" var="msgs" />
                
                <webuijsf:head title="#{msgs.login_title}">
                </webuijsf:head>
                
                <webuijsf:body>
                    
                    <webuijsf:contentPageTitle title=""/>
                    
                    <table border="0" cellpadding="0" cellspacing="0" align="center">
                        <tr>
                            <td width="50%"><img src="/cam/theme/com/sun/webui/jsf/suntheme/images/other/dot.gif" width="1" height="1" alt="" /></td>
                            <td><img src="/cam/theme/com/sun/webui/jsf/suntheme/images/other/dot.gif" width="728" height="1" alt="" /></td>
                            <td width="50%"><img src="/cam/theme/com/sun/webui/jsf/suntheme/images/other/dot.gif" width="1" height="1" alt="" /></td>
                        </tr>
                        <tr style="background-color:#4A5C69;background-image: url(/cam/theme/com/sun/webui/jsf/suntheme/images/login/gradlogtop.jpg); background-repeat: repeat-x; background-position: left top;">
                            <td><img src="/cam/theme/com/sun/webui/jsf/suntheme/images/other/dot.gif" width="1" height="30" alt="" /></td>
                            <td nowrap="nowrap" valign="middle"><img src="/cam/theme/com/sun/webui/jsf/suntheme/images/login/dot.gif" width="1" height="30" alt="" /></td>
                            <td><img src="/cam/theme/com/sun/webui/jsf/suntheme/images/other/dot.gif" width="1" height="30" alt="" /></td>
                        </tr>
                        
                        <tr>
                            <td style="background-color:#D4DCE1;background-image: url(/cam/theme/com/sun/webui/jsf/suntheme/images/login/gradlogsides.jpg);background-repeat:repeat-x;background-position:left top;">
                            </td>
                            <td style="background-color:#FFFFFF;background-image: url(/cam/theme/com/sun/webui/jsf/suntheme/images/login/login-backimage.jpg);background-repeat:no-repeat;background-position:left top;" height="435" align="center" valign="middle">
                                
                                <table border="0" cellpadding="2" cellspacing="2" style="margin-left:200px;margin-top:100px;">

                                    <tr style="margin-top:20px;margin-bottom:5px">
                                        <td><font face="Arial,Helvetica"/>
                                            <webuijsf:alert id="msg1" type="information" summary="#{msgs.login_auth_failed}" 
                                            detail="#{msgs.login_reenter}" />
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="center" valign="middle"><font face="Arial,Helvetica"/><b><webuijsf:staticText id="CamLogin" text="#{msgs.login_title}" style="color:#639C9C;font-size:large;" /></b>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="center" valign="top">
                                            <form method="post" action="j_security_check">
                                                <table border="0" align="center" cellpadding="2" cellspacing="5" style="margin-top:20px;">
                                                    <tr>
                                                        <td><font face="Arial,Helvetica"/><b><webuijsf:staticText id="userName" text="#{msgs.login_username}" /></b></td>
                                                        <td>
                                                        <input type="text" name="j_username" style="width:150px" /></td>
                                                    </tr>
                                                    
                                                    <tr style="margin-top:5px;margin-bottom:5px">
                                                        <td><font face="Arial,Helvetica"/><b><webuijsf:staticText id="password" text="#{msgs.login_password}" /></b></td>
                                                        <td><input type="password" name="j_password" style="width:150px" /></td>
                                                    </tr>
                                                    <tr style="margin-top:5px;margin-bottom:5px">
                                                        <td colspan="2" align="right">
                                                            <webuijsf:button id="login" text="#{msgs.login_login}" />
                                                        </td>
                                                    </tr>
                                                </table>
                                            </form>
                                        </td>
                                    </tr>
                                </table>
                            </td>
                            
                            <td style="background-color:#D4DCE1;background-image: url(/cam/theme/com/sun/webui/jsf/suntheme/images/login/gradlogsides.jpg);background-repeat:repeat-x;background-position:left top;">
                            </td>
                            
                            
                        </tr>
                        <tr style="background-color:#4A5C69;background-image: url(/cam/theme/com/sun/webui/jsf/suntheme/images/login/gradlogbot.jpg);background-repeat:repeat-x;background-position:left top;">
                            <td> </td>
                            <td><div style="color:#FFFFFF;width:720px;margin:5px 0px 50px;vertical-align:top;"><span style="font-size:9pt;vertical-align:top;">
                            <webuijsf:staticText text="#{msgs.login_copyright}" /></span></div></td>
                            <td> </td>
                        </tr>
                    </table>
                    
                    
                </webuijsf:body>
            </webuijsf:html>
        </webuijsf:page>
    </f:view>
</jsp:root>

