/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)SVGRendererBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.renderers.svg;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.connectors.LocalServerConnector;
import com.sun.jbi.ui.client.JBIAdminCommandsClientFactory;
import com.sun.jbi.ui.common.JBIAdminCommands;
import com.sun.jbi.ui.common.JBIRemoteException;
import java.util.logging.Logger;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.management.MBeanServerConnection;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

/**
 *
 * @author rdamir
 */
public class SVGRendererBean {
   
    private static final String BROWSER = "browser";
    private static final String MS_IE = "msie";
    private static final String FIREFOX = "firefox";
    private static final String IE = "Internet Explorer";
    private static final String MOZILLA = "Mozilla";
    
    private Logger logger = 
            Logger.getLogger(SVGRendererBean.class.getName());
    
    private String serviceUnitURL;
    private String serviceAssemblyURL;

    public SVGRendererBean() {
    }
 
    public String getSVGString(String saName,String targetName) {
      String svgStr = null;
      JBIDeploymentDescriptorProcessor ddp = new 
            JBIDeploymentDescriptorProcessor(targetName);
        try {
            svgStr = ddp.processJBIAssemblyDescriptors(saName,targetName);
        } catch (Exception ex) {
            svgStr = ex.getMessage();
        }
      return svgStr;
  
    }

    public void setServiceUnitURL() {
    }    
    public String getServiceUnitURL() {
        FacesContext context = FacesContext.getCurrentInstance();
        ExternalContext ex = context.getExternalContext();
        HttpServletRequest request = (HttpServletRequest)ex.getRequest();
        HttpSession session = request.getSession();           
        
        String componentName = 
                (String)session.getAttribute(GenericConstants.COMPONENT_NAME);
        String componentType= 
                (String)session.getAttribute(GenericConstants.COMPONENT_TYPE);
        if(componentType.equalsIgnoreCase(GenericConstants.SA_TYPE)) {
            serviceUnitURL = GenericConstants.BLANK_SERVICE_UNIT_URL;
        } else {
            String cType =
                (String)session.getAttribute(GenericConstants.COMPONENT_CTYPE);
            String  cName = 
                (String)session.getAttribute(GenericConstants.COMPONENT_CNAME);
            String  tName = 
                (String)session.getAttribute(GenericConstants.COMPONENT_TNAME);
            serviceUnitURL =  new StringBuffer(GenericConstants.SERVICE_UNIT_URL + 
                GenericConstants.COMPONENT_NAME + "=" + componentName + "&" +
                GenericConstants.COMPONENT_TYPE + "=" + componentType + "&" +
                GenericConstants.COMPONENT_CTYPE + "=" + cType + "&" +
                GenericConstants.COMPONENT_CNAME + "=" + cName + 
                GenericConstants.COMPONENT_TNAME + "=" + tName).toString();
        }
        logger.info("serviceUnitURL = " + serviceUnitURL);
        return serviceUnitURL;
    }
    
    public void setServiceAssemblyURL() {
    }
    
    public String getServiceAssemblyURL() {
        String viewer ="";
        FacesContext context = FacesContext.getCurrentInstance();
        ExternalContext ex = context.getExternalContext();
        HttpServletRequest request = (HttpServletRequest)ex.getRequest();
        HttpSession session = request.getSession();           
        
        
        String browser = request.getParameter(BROWSER);
        if(browser != null) {
            session.setAttribute(BROWSER,browser);
        } else {
            browser = (String)session.getAttribute(BROWSER);
        }
        
        if(browser.equals(MS_IE)) {
            viewer = GenericConstants.SERVICE_ASSEMBLY_IE_URL;
        } else {
             viewer = GenericConstants.SERVICE_ASSEMBLY_FIREFOX_URL;
        }
        String appName = 
                (String)session.getAttribute(GenericConstants.APP_NAME);
        String  tName = 
            (String)session.getAttribute(GenericConstants.COMPONENT_TNAME);
        serviceAssemblyURL =
            new StringBuffer(viewer + "?"+ 
              GenericConstants.SERVICE_ASSEMBLY_NAME + "=" + appName 
              + "&" + GenericConstants.COMPONENT_TYPE + "=" + GenericConstants.SA_TYPE
              + "&" + GenericConstants.COMPONENT_TNAME + "=" + tName
              ).toString();
     
        logger.info("serviceAssemblyURL = " + serviceAssemblyURL);
        return serviceAssemblyURL;
    }
   
    public String getServiceAssemblyControlURL() {
      return GenericConstants.SERVICE_ASSEMBLY_CONTROL_URL;
    }
    
     public String getServiceAssemblyJBITextURL() {
      return GenericConstants.SERVICE_ASSEMBLY_TEXTVIEWER_URL;
    }

   public String getDownloadViewerMessage() {
        String viewerMessage =  Messages.getString("svg_browser_no_supported");
        FacesContext context = FacesContext.getCurrentInstance();
        ExternalContext ex = context.getExternalContext();
        HttpServletRequest request = (HttpServletRequest)ex.getRequest();
        
        String browserName = 
                (String)request.getParameter(BROWSER);
        if(MOZILLA.equalsIgnoreCase(browserName)) {
           viewerMessage =  Messages.getString("svg_mozilla_SVGVieiwer");
        } else if (IE.equalsIgnoreCase(browserName)) {
           viewerMessage =  Messages.getString("svg_IE_SVGVieiwer");
        }         
        logger.info("browser ID = " + browserName);
        return viewerMessage;
    }
    
  public String getJBITexttitle() {
        FacesContext context = FacesContext.getCurrentInstance();
        ExternalContext ex = context.getExternalContext();
        HttpServletRequest request = (HttpServletRequest)ex.getRequest();
        HttpSession session = request.getSession();           
        
        String componentName = 
                (String)session.getAttribute(GenericConstants.COMPONENT_NAME);
        return componentName+ " JBI.XML";
  }
  
  public String getJBIXmlText() {
       StringBuffer jbiXMLContent =  new StringBuffer();
       FacesContext context = FacesContext.getCurrentInstance();
       ExternalContext ex = context.getExternalContext();
       HttpServletRequest request = (HttpServletRequest)ex.getRequest();
       HttpSession session = request.getSession();           
        
       String saName = 
                (String)session.getAttribute(GenericConstants.COMPONENT_NAME);
       
       // get mbeanserver connection - the following 
       // assume the app. running on the same machine,
       LocalServerConnector connector = new LocalServerConnector();
        try {
            
            MBeanServerConnection conn = connector.getConnection();
            JBIAdminCommands adminCmd = 
                    JBIAdminCommandsClientFactory.getInstance(conn);
            jbiXMLContent.append(adminCmd.getServiceAssemblyDeploymentDescriptor(saName));
           
        } catch (JBIRemoteException jbiRemoteex) {
            jbiRemoteex.printStackTrace();
            return Messages.getString("sa_fail_to_get_jbixml");
        }
       return jbiXMLContent.toString();
  }
}
