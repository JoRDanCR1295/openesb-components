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
 * @(#)JBIDeploymentDescriptorProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.renderers.svg;

import com.sun.jbi.cam.manager.framework.common.DeploymentDescriptorsProcessor;
import com.sun.jbi.cam.manager.framework.common.JBIServiceInformation;
import com.sun.jbi.cam.manager.framework.common.Util;
import com.sun.jbi.cam.services.ServiceManager;
import com.sun.jbi.cam.services.ServiceManagerFactory;
import java.io.DataInput;
import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.zip.ZipFile;
import java.util.logging.Logger;


import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.apache.xpath.XPathAPI;
import com.sun.jbi.cam.services.administration.AdministrationService;
import java.util.Map;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import com.sun.jbi.cam.model.management.JBIServiceUnitStatus;

/**
 * @author Sun MicrosystemInc.
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class JBIDeploymentDescriptorProcessor {
    

    private String appName ;
    private JBIConnectionsInformation jbiConnectionsInformation;
    private List<JBIServiceInformation> serviceInfoList;
    private AdministrationService adminService;
    private Logger logger = 
            Logger.getLogger(JBIDeploymentDescriptorProcessor.class.getName());
    private boolean isLegendVisible = true;
    private List<JBIServiceUnitStatus> suStatusList;
    private String targetName;

    /**
     * 
     */
    public JBIDeploymentDescriptorProcessor(String targetName) {
        // todo - use real componentName and componentType
        ServiceManager mgr = ServiceManagerFactory.getServiceManager("componentName","componentType");
        adminService = mgr.getAdministrationService(targetName);
        //adminService = new AdministrationService( new LocalServerConnector());
   }

   public JBIDeploymentDescriptorProcessor(boolean isLegendVisible,
          List<JBIServiceUnitStatus> suStatusList,String targetName) {
        this(targetName);
        this.isLegendVisible = isLegendVisible;
        this.suStatusList = suStatusList;
        this.targetName = targetName;
   }

    // entry point if the JBI CA zip file is provided
    private void processJBIApplicationDeploymentDescriptor(ZipFile compositeAppZip)
    	throws Exception{

        Element root = Util.getXMLDocumentRoot(compositeAppZip,
                "META-INF/jbi.xml");
        Node appNameNode = XPathAPI.selectSingleNode(root,
                "//service-assembly/identification/name");
        appName = Util.getNodeTextValue(appNameNode);
        serviceInfoList = getServicesInformation(root);
        populateServiceObjects(serviceInfoList,compositeAppZip);
        jbiConnectionsInformation =  new JBIConnectionsInformation();
        jbiConnectionsInformation.processConnectionInformation(root);
    }

    // enrty point if the CA name is provided. It uses the knowledage of where
    // the CA is expended in glassfish. (it will replaced with MBean queries
    // for the jbi DD once implemented (glassfish 9.1 per Gopalan).
    public String processJBIAssemblyDescriptors(String 
            compositeAppAssemblyName,String targetName) throws Exception{
  
        DeploymentDescriptorsProcessor ddp = new 
                DeploymentDescriptorsProcessor(targetName);
        ddp.initialize(compositeAppAssemblyName);
        

       serviceInfoList = ddp.getServicesUnitsInformation(compositeAppAssemblyName,null);

        populateServiceObjects(serviceInfoList);
        updateServicesObjectStatus();
      
        jbiConnectionsInformation =  new JBIConnectionsInformation();
        jbiConnectionsInformation.processConnectionInformation(ddp.getServiceAssemblyRootElement());
        // done with metadata collection - start the rendering process.
        // the end result is xml string contain SVG information.
        SVGRenderer renderer =  
            new SVGRenderer("",isLegendVisible,targetName);
        
        String svgXmlData= renderer.generateSVG(getServiceInfoList(),
                getJbiConnectionsInformation());
        
//        logger.fine("\n SVG XML data: \n" + svgXmlData);
        logger.info("\n SVG XML data: \n" + svgXmlData);
         
        return svgXmlData;

    }
    // to be modify after the descriptors will be aviable via
    // Gopalan API
    private ArrayList<String>  getServiceUnitsDir(String compositeAppRoot) {
        ArrayList<String> suDirList = new ArrayList<String>(); 
        File rootDir = new File(compositeAppRoot);
        File[] filesInRootDir = rootDir.listFiles();
        if(filesInRootDir == null) {
            return suDirList; // invalid composite root path probably invalid
                              // composite app name
        }
        for (int index=0 ; index < filesInRootDir.length; index++) {
            File file = filesInRootDir[index];
            if(file.isDirectory()) {
                // top level meta inf already process
                if(file.getAbsolutePath().indexOf("META-INF") == -1) {
                    suDirList.add(file.getAbsolutePath());
                }
                
            }        
        }
        return   suDirList;  
    }

      
    private void populateServiceObjects(List serviceList) throws Exception {
        for (Iterator iter = serviceList.iterator(); iter.hasNext();) {
            JBIServiceInformation serviceInformation = 
                (JBIServiceInformation) iter.next();
            
            serviceInformation.processServiceDescriptor();
        }
        
    }
   
    
    private void populateServiceObjects(List serviceList,
            ZipFile compositeAppZip) throws Exception {
        for (Iterator iter = serviceList.iterator(); iter.hasNext();) {
            JBIServiceInformation serviceInformation = 
                (JBIServiceInformation) iter.next();
            String jarFileName = serviceInformation.getJarFileName();
            serviceInformation.processServiceDescriptor(compositeAppZip,
                    jarFileName);
        }
        
    }
    
   
    private List<JBIServiceInformation> getServicesInformation(Node root) throws Exception{
        List<JBIServiceInformation> serviceInfoList  = new ArrayList<JBIServiceInformation>();
        
        NodeList servicesList = XPathAPI.selectNodeList(root,
                "//service-assembly/service-unit");
        for (int i = 0; i < servicesList.getLength(); i++) {
            Node serviceUnit = servicesList.item(i);
            Node serviceUnitNameNode = XPathAPI.selectSingleNode(serviceUnit,
               "identification/name");
            String serviceUnitName = Util.getNodeTextValue(serviceUnitNameNode);
            Node compNameNode = XPathAPI.selectSingleNode(serviceUnit,
                    "target/component-name");
            String componentName = Util.getNodeTextValue(compNameNode);
            Node zipNode = XPathAPI.selectSingleNode(serviceUnit, "artifacts-zip");
            String zipFileName = Util.getNodeTextValue(zipNode);
            JBIServiceInformation serviceInfo = 
                new JBIServiceInformation(zipFileName,
                             componentName, serviceUnitName,false,targetName);
            serviceInfoList.add(serviceInfo);
        }
        return serviceInfoList;
    }
    
    
    private void updateServicesObjectStatus() {
        
        if ( suStatusList!=null ) {
            Iterator<JBIServiceInformation> iter = serviceInfoList.iterator();
            while (iter.hasNext()) {
                JBIServiceInformation si = iter.next();
                String componentName = si.getComponentName();
                Iterator<JBIServiceUnitStatus> suIter = suStatusList.iterator();
                while( suIter.hasNext() ) {
                    JBIServiceUnitStatus suStatus = suIter.next();
                    String targetName = suStatus.getTargetName();
                    if(targetName.equals(componentName)) {
                        si.setStatus(suStatus.getStatus());
                        break;
                    }
                }
            }
        }
    }
    
    /**
     * @return Returns the appName.
     */
    public String getSVGFileName() {
        return appName;
    }
    
    /**
     * @return Returns the jbiConnectionsInformation.
     */
    public JBIConnectionsInformation getJbiConnectionsInformation() {
        return jbiConnectionsInformation;
    }
    /**
     * @return Returns the serviceInfoList.
     */
    public List getServiceInfoList() {
        return serviceInfoList;
    }
    
//    private AdministrationService 
    
    public static void main(String[] args) {
        JBIDeploymentDescriptorProcessor jbiDDProcessor = 
            new JBIDeploymentDescriptorProcessor("server");
        DataInput in = new java.io.DataInputStream(System.in);
        boolean valid = false;
        try {
            while (!valid) {
                System.out.print("enter the composite app (zip file) path:");
                String name = in.readLine();
                File f = new File(name);
                if(f.exists()) {
                    valid = true;
                    ZipFile compositeAppZip = new ZipFile(name);
                    jbiDDProcessor.processJBIApplicationDeploymentDescriptor(compositeAppZip);
                    SVGRenderer renderer =  
                        new SVGRenderer(jbiDDProcessor.getSVGFileName(),true,"");
                    renderer.generateSVGFile(jbiDDProcessor.getServiceInfoList(),
                            jbiDDProcessor.getJbiConnectionsInformation());
                }
            }
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }        
    }
}
