<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:webuijsf="http://www.sun.com/webui/webuijsf">

    <jsp:directive.page contentType="text/html;charset=ISO-8859-1" pageEncoding="UTF-8"/>
    
    <f:view>
        <webuijsf:page frame="true">
            <webuijsf:html>
                <webuijsf:head title="Woodstock" />
                <webuijsf:frameSet id="tabFrameSet"  rows="50px,*"  >
                     <webuijsf:frame name="tabTopFrame" toolTip="topFrame" url="tabSet.jsp" frameBorder="false" noResize="false" scrolling="false" />
                     <webuijsf:frame name="tabBottomFrame" frameBorder="false" url=""  noResize="false"/>
                </webuijsf:frameSet>
            </webuijsf:html>
        </webuijsf:page>
    </f:view>

</jsp:root>
