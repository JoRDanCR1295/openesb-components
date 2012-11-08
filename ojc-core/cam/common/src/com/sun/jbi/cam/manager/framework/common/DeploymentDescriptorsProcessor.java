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
 * @(#)DeploymentDescriptorsProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.common;

import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.services.ServiceManager;
import com.sun.jbi.cam.services.ServiceManagerFactory;
import java.io.File;
import java.util.zip.ZipFile;
import javax.management.ObjectName;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.apache.xpath.XPathAPI;
import com.sun.jbi.cam.services.administration.AdministrationService;
import com.sun.jbi.ui.client.JBIAdminCommandsClientFactory;
import com.sun.jbi.ui.common.JBIAdminCommands;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.w3c.dom.Node;

/**
 *
 * @author rdamir
 */
public class DeploymentDescriptorsProcessor {
    
    private AdministrationService adminService;
    private String appName ;
    private String saDescriptorPath;
    private String compositeAppRoot;
    private Element saRoot;
    private String targetName;
    
    /** Creates a new instance of DeploymentDescriptorsProcessor */
    public DeploymentDescriptorsProcessor(String targetName) {
        ServiceManager mgr = ServiceManagerFactory.getServiceManager("componentName","componentType");
        adminService = mgr.getAdministrationService(targetName);
        this.targetName = targetName;
   }
    
   // enrty point if the CA name is provided. It uses the knowledage of where
   // the CA is expended in glassfish. (it will replaced with MBean queries
   // for the jbi DD once implemented (glassfish 9.1 per Gopalan).
   public void initialize(String  ServiceAssemblyName) throws Exception{
        initProcessingForSA(ServiceAssemblyName);
    }
    
    private void initProcessingForSA(String 
            ServiceAssemblyName) throws Exception{
        
        
        if(ServiceAssemblyName == null) {
            throw new IllegalArgumentException(Messages.getString(
                    "svg.render.invalid.Composite.App.Name"));
        }
        appName =  ServiceAssemblyName;
        
        //@to do replace the following logic once Gopalan 
        // implement the API for the jbi descriptors for
        // all the components of the CA.
        // refactor for GF91 - jbi.xml is not available - use zipfile

        
        String saJbiXML = 
             adminService.getServiceAssemblyDeploymentDescriptor(ServiceAssemblyName);
        
         saRoot = Util.getXMLDocumentRoot(saJbiXML);
       
    }
 
    public String getServiceAssemblyRootPath() {
        return compositeAppRoot;
    }

    public Element getServiceAssemblyRootElement() {
        return saRoot;
    }
    
    /*
     * the method create a list of JBIServiceInformation instances and return 
     * it to the caller if suName is provided (not null or empty) the list
     * contain only the instance that has service unit name that match 
     * the parameter.
     */
    public List<JBIServiceInformation> getServicesUnitsInformation(String serviceAssemblyName,
            String suName) throws Exception{
        List<JBIServiceInformation> serviceInfoList  = new ArrayList<JBIServiceInformation>();
        NodeList servicesList = XPathAPI.selectNodeList(saRoot,
                "//service-assembly/service-unit");
        for (int i = 0; i < servicesList.getLength(); i++) {
            Node serviceUnit = servicesList.item(i);
            Node serviceUnitNameNode = XPathAPI.selectSingleNode(serviceUnit,
                "identification/name");
            String serviceUnitName = Util.getNodeTextValue(serviceUnitNameNode);
            Node compNameNode = XPathAPI.selectSingleNode(serviceUnit,
                    "target/component-name");
            String componentName = Util.getNodeTextValue(compNameNode);
            if((suName != null)) {
                if (serviceUnitName.endsWith(suName)) {
                  JBIServiceInformation serviceInfo = 
                    new JBIServiceInformation(componentName, 
                        serviceUnitName,false,targetName,serviceAssemblyName);
                    serviceInfoList.add(serviceInfo);
                  break;
                }
               continue; 
            } else {
                JBIServiceInformation serviceInfo = 
                    new JBIServiceInformation(componentName, 
                        serviceUnitName,false,targetName,serviceAssemblyName);
                serviceInfoList.add(serviceInfo);
            }
        }
        return serviceInfoList;
    }

     public void populateServiceObjects(List serviceList) throws Exception {
         
        for (Iterator iter = serviceList.iterator(); iter.hasNext();) {
            JBIServiceInformation serviceInformation = 
                (JBIServiceInformation) iter.next();
            
            serviceInformation.processServiceDescriptor();        
        }
        
    }

    
    
    public Map<String,String> getConsumesEndPointListForSU(String saName,
            String suName) throws Exception {
        return getEndPointMaptForSU(saName,suName,true);
    }
    
    
    public Map<String,String> getProvidesEndPointMapForSU(String saName,
           String suName) throws Exception {
        return getEndPointMaptForSU(saName,suName,false);
        
    }
    
     public Map<String,String> getEndPointMaptForSU(String saName,
             String suName,boolean isConsume) throws Exception {
        Map<String,String> endPointsDataMap= new HashMap<String,String>(); 
        initProcessingForSA(saName);
        List suList = getServicesUnitsInformation(saName,suName);

        populateServiceObjects(suList);
        if(!suList.isEmpty()) {
          Map<String,JBIServiceUnitInformation> epMap = null;  
          JBIServiceInformation jbisi =  (JBIServiceInformation)suList.get(0);
          if(isConsume) {
             epMap=jbisi.getConsumesMap();
          } else {
             epMap=jbisi.getProvidesMap();
          }
          buildEndPointsMap(epMap,endPointsDataMap);
        }
        
        return endPointsDataMap;
        
    }
    
  
     private void buildEndPointsMap(Map<String,JBIServiceUnitInformation> epMap,
             Map<String,String> endPointDataMap) {
         
        Set epMapKeySet = epMap.keySet();
        Iterator iter = epMapKeySet.iterator();
        while (iter.hasNext()) {
            String key = (String)iter.next();
            JBIServiceUnitInformation sui = epMap.get(key);
            String fqsn = sui.getFullyQualifiedserviceName();
            fqsn = fqsn.replace("^",",");
            String epName = sui.getEndpointName();
            String epCompositeName = fqsn +"," + epName;
            endPointDataMap.put(epCompositeName,epCompositeName);
        }
         
     }
    
    // to be modify after the descriptors will be aviable via
    // Gopalan API
    public ArrayList<String>  getServiceUnitsDir() {
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
   
    
    
    public static void main(String[] args) {
        DeploymentDescriptorsProcessor DDProcessor = 
            new DeploymentDescriptorsProcessor("server");
        
        try {
            Map cmap = DDProcessor.getConsumesEndPointListForSU("FileBCJbi",
                "com.sun.filebc-1.0-2");
            
            System.out.println(cmap.keySet().toString());
            Map pmap = DDProcessor.getProvidesEndPointMapForSU("FileBCJbi",
            "com.sun.filebc-1.0-2");
            System.out.println(pmap.keySet().toString());
 
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }        
    }

}
