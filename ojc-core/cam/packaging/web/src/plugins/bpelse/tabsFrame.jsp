<?xml version="1.0" encoding="UTF-8"?>

<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">

    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
    
    <jsp:scriptlet> 

       String name = request.getParameter("name");
       String type = request.getParameter("type");
       String ctype = request.getParameter("ctype");
       String cname = request.getParameter("cname");
       String pname = request.getParameter("pname");
       String tname = request.getParameter("tname");

       session.setAttribute("name",name);
       session.setAttribute("type",type);
       session.setAttribute("ctype",ctype);
       session.setAttribute("cname",cname);
       session.setAttribute("pname",pname);
       session.setAttribute("tname",tname);

    </jsp:scriptlet>     
    
    <f:view>
        <webuijsf:page frame="true">
            <webuijsf:html>
                
                <webuijsf:head title="Composite Application Manager"/>
                
                <webuijsf:frameSet id="tabFrameSet"  rows="50px,*"  >
                     <webuijsf:frame name="tabTopFrame" toolTip="topFrame" url="/faces/plugins/bpelse/tabs.jsp" frameBorder="false" noResize="true" scrolling="false" />
                    <!--
                    <webuijsf:frame name="tabBottomFrame" frameBorder="false" url="/faces/plugins/bpelse/instances.jsp"  noResize="false"/>
                    -->
                    <webuijsf:frame name="tabBottomFrame" frameBorder="false" url=""  noResize="false"/>
                </webuijsf:frameSet>
            </webuijsf:html>
        </webuijsf:page>
    </f:view>

</jsp:root>

