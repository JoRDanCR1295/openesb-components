<%@page pageEncoding="UTF-8" contentType="text/html; charset=UTF-8"%>
<%@page import="com.sun.workflow.xform.*"%>
<%@page import="com.sun.workflow.xml.*"%>
<%@page import="java.io.*"%>
<%@page import="org.w3c.dom.Node"%>

<%--
The taglib directive below imports the JSTL library. If you uncomment it,
you must also add the JSTL library to the project. The Add Library... action
on Libraries node in Projects view can be used to add the JSTL 1.1 library.
--%>

<%@taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c"%> 

<%--
    This example uses JSTL, uncomment the taglib directive above.
    To test, display the page like this: index.jsp?sayHello=true&name=Murphy
    --%>
    <%--
    <c:if test="${param.sayHello}">
        <!-- Let's welcome the user ${param.name} -->
        Hello ${param.name}!
    </c:if>
    --%>


<% response.setContentType("application/xhtml+xml; charset=UTF-8");
String taskId = request.getParameter("taskId");
String claimedBy = request.getParameter("claimedBy");
String userId = request.getUserPrincipal().getName();
TaskXFormViewBuilder builder = new TaskXFormViewBuilder(taskId, claimedBy, userId);
Node xformView = builder.build();
if (xformView != null) {
    String xml = Util.toXml(xformView, "UTF-8", true);

    if (xml != null) {
//        xml = new String(xml.getBytes("ISO-8859-1"),"UTF-8");
        xml = Util.formatData(xml);
        out.write(xml);
//        byte[] utf8Bytes = xml.getBytes("UTF8");
//        FileOutputStream fout= new FileOutputStream("XformOutput.xml");

// Open an output stream

//        fout.write(utf8Bytes);
//        fout.flush();
//        fout.close();
    }
}

%>
