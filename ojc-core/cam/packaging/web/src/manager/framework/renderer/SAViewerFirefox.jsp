<% response.setContentType("image/svg+xml; charset=UTF-8");%>  

<%@ page import="com.sun.jbi.cam.common.GenericConstants,
   com.sun.jbi.cam.manager.framework.renderers.svg.JBIDeploymentDescriptorProcessor,
   com.sun.jbi.cam.manager.framework.generic.ServiceUnitsBean,
   java.util.List,
   com.sun.jbi.cam.model.management.JBIServiceUnitStatus" 
%>
   
<%
    String targetName = 
            request.getParameter(GenericConstants.COMPONENT_TNAME);
    String assemblyName = 
            request.getParameter(GenericConstants.SERVICE_ASSEMBLY_NAME);
    String isVisibleVal = (String)session.getAttribute("legendVisible");
    // default is visible if the attribute does not exist
    boolean isVisible = (isVisibleVal=="true" || isVisibleVal == null)
            ? true : false;
    ServiceUnitsBean susBean =  new ServiceUnitsBean();
    List<JBIServiceUnitStatus> suStatusList = 
            susBean.getSAServiceUnitStatusList(assemblyName,targetName);
    JBIDeploymentDescriptorProcessor ddp = new 
            JBIDeploymentDescriptorProcessor(isVisible,suStatusList,targetName);
    String svgNewStr  = ddp.processJBIAssemblyDescriptors(assemblyName,targetName);
%>


<%= svgNewStr %>
