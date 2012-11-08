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
 * @(#)JBIServiceInformation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.common;

import com.sun.jbi.cam.manager.framework.renderers.svg.*;
import com.sun.jbi.cam.services.ServiceManager;
import com.sun.jbi.cam.services.ServiceManagerFactory;
import com.sun.jbi.cam.services.administration.AdministrationService;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.jar.JarFile;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;


import org.apache.xpath.XPathAPI;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Sun MicrosystemInc.
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class JBIServiceInformation {
    
    private static final int BUFFER_SIZE = 2048;
    private Logger logger = 
            Logger.getLogger(JBIServiceInformation.class.getName());

    private String jarFileName;
    private String componentName;
    private String serviceUnitName;
    private boolean bindComponent;
    private Map<String,JBIServiceUnitInformation> consumesMap;
    private Map<String,JBIServiceUnitInformation> providesMap;
    private Map nameSpaces;
    private String status;
    private String targetName;
    private AdministrationService adminService;
    private String serviceAssemblyName;

    /**
     * @param componentName
     * @param bindComponent
     * @param consumesEndpointName
     * @param consumesServiceName
     * @param consumesInterfaceName
     */
    public JBIServiceInformation(String componentName,
                    String serviceUnitName, boolean bindComponent,
                    String targetName,String serviceAssemblyName) {
        super();
        this.componentName = componentName;
        this.bindComponent = bindComponent;
        this.serviceUnitName = serviceUnitName;
        consumesMap  = new HashMap<String,JBIServiceUnitInformation>();
        providesMap = new HashMap<String,JBIServiceUnitInformation>();
        nameSpaces = new HashMap();
        this.targetName = targetName;
        this.serviceAssemblyName = serviceAssemblyName;
        ServiceManager mgr = ServiceManagerFactory.getServiceManager("componentName","componentType");
        adminService = mgr.getAdministrationService(targetName);
   }

    /**
     * @param jarFileName
     * @param componentName
     * @param bindComponent
     * @param consumesEndpointName
     * @param consumesServiceName
     * @param consumesInterfaceName
     */
    public JBIServiceInformation(String jarFileName, String componentName,
                    String serviceUnitName, boolean bindComponent,
                    String targetName ) {
        super();
        this.jarFileName = jarFileName;
        this.componentName = componentName;
        this.bindComponent = bindComponent;
        this.serviceUnitName = serviceUnitName;
        consumesMap  = new HashMap<String,JBIServiceUnitInformation>();
        providesMap = new HashMap<String,JBIServiceUnitInformation>();
        nameSpaces = new HashMap();
        this.targetName = targetName;

    }
    /**
     * @return Returns the bindComponent.
     */
    public boolean isBindComponent() {
        return bindComponent;
    }
    /**
     * @param bindComponent The bindComponent to set.
     */
    public void setBindComponent(boolean bindComponent) {
        this.bindComponent = bindComponent;
    }
    /**
     * @return Returns the componentName.
     */
    public String getComponentName() {
        return componentName;
    }
    /**
     * @param componentName The componentName to set.
     */
    public void setComponentName(String componentName) {
        this.componentName = componentName;
    }
    /**
     * @return Returns the jarFileName.
     */
    public String getJarFileName() {
        return jarFileName;
    }
    /**
     * @param jarFileName The jarFileName to set.
     */
    public void setJarFileName(String jarFileName) {
        this.jarFileName = jarFileName;
    }

    /**
     * @return Returns the consumes.
     */
    public boolean hasConsumes() {
        return consumesMap.size() > 0;
    }
    /**
     * @return Returns the provides.
     */
    public boolean hasProvides() {
        return providesMap.size() > 0;
    }
    
    public void processServiceDescriptor(ZipFile compositeAppZip,
                    String jarFileName) throws Exception {
        // extract the jar file from the stream to user dir.
        String tempJarDir = System.getProperty("user.home")+File.separator+ jarFileName;

        ZipEntry ze = compositeAppZip.getEntry(jarFileName);
        BufferedInputStream bis = new BufferedInputStream(compositeAppZip.getInputStream(ze));
        FileOutputStream fos = new FileOutputStream(tempJarDir);
        long jarFileSize = ze.getSize();
        byte[] buffer = new byte[BUFFER_SIZE];
        int byteRead = 0;
        while (true) {
            byteRead = bis.read(buffer);
            if(byteRead == -1) {
                break;
            } else if (byteRead < BUFFER_SIZE){
                fos.write(buffer,0,byteRead);
                break;
            } else {
                fos.write(buffer);
            }
              
         }
        fos.flush();
        fos.close();
        
        JarFile jf = new JarFile(tempJarDir);
        Element root = Util.getXMLDocumentRoot(jf,"META-INF/jbi.xml");
        extractDataFromServiceUnit(root);
        jf.close();
        boolean deleted = (new File(tempJarDir)).delete();
    }


   public void processServiceDescriptor() throws Exception {
       String suJbiXML = 
             adminService.getServiceUnitDeploymentDescriptor(serviceAssemblyName,
              serviceUnitName);
        
        Element root  = Util.getXMLDocumentRoot(suJbiXML);
        extractDataFromServiceUnit(root);
    }
    
    private void extractDataFromServiceUnit(Element root) throws Exception{
        JBIServiceUnitInformation info = null;
        nameSpaces = Util.getNameSpaces(root);
        Node bindingComponentNode = XPathAPI.selectSingleNode(root,"//services/@binding-component");
        String bindingComponentValue = bindingComponentNode.getNodeValue();
        boolean isBindingComponent = Boolean.valueOf(bindingComponentValue).booleanValue();
        setBindComponent(isBindingComponent);
        NodeList nodesList = XPathAPI.selectNodeList(root,"//services/consumes");
        for (int index = 0; index < nodesList.getLength(); index++) {
            Node node = nodesList.item(index);
            info = getConsumesProvidesInfo (node);
            consumesMap.put(info.toString(),info);    
        }

        nodesList = XPathAPI.selectNodeList(root,"//services/provides");
        for (int index = 0; index < nodesList.getLength(); index++) {
            Node node = nodesList.item(index);
            info = getConsumesProvidesInfo (node);
            providesMap.put(info.toString(),info);    
        }
        
    }

    private JBIServiceUnitInformation getConsumesProvidesInfo(Node node) throws Exception{
        Node attribNode = XPathAPI.selectSingleNode(node,"./@endpoint-name");
        String endpointNameValue = attribNode.getNodeValue();
        attribNode = XPathAPI.selectSingleNode(node,"./@service-name");
        String serviceNameValue = attribNode.getNodeValue();
        String FQserviceNameValue = Util.resolveNameSpace(nameSpaces,serviceNameValue);
        attribNode = XPathAPI.selectSingleNode(node,"./@interface-name");
        String interfaceNameValue = attribNode.getNodeValue();
        JBIServiceUnitInformation jbiServiceUnitInformation = 
            new JBIServiceUnitInformation(endpointNameValue,serviceNameValue,
                    FQserviceNameValue,interfaceNameValue);
        return jbiServiceUnitInformation;
       
    }
    
    
    public Map<String,JBIServiceUnitInformation> getConsumesMap() {
        return consumesMap;
    }
    
    public Map<String,JBIServiceUnitInformation> getProvidesMap() {
        return providesMap;
    }
   
    /**
     * @return Returns the serviceUnitName.
     */
    public String getServiceUnitName() {
        return serviceUnitName;
    }
    
    public String getStatus() {
        return status;
    }
    
    public void setStatus(String status) {
        this.status = status;
    }
    
    public String getTargetName() {
        return targetName;
    }


 }
