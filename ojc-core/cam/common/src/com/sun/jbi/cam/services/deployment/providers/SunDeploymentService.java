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
 * @(#)SunDeploymentService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.services.deployment.providers;

import com.sun.jbi.cam.connectors.ServerConnector;
import com.sun.jbi.cam.services.BaseServiceProvider;
import com.sun.jbi.cam.services.deployment.DeploymentService;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.io.File;
import javax.management.ObjectName;
//import com.sun.enterprise.deployment.backend.DeploymentStatus;

/**
 *
 * @author ylee
 */
public class SunDeploymentService extends BaseServiceProvider implements Serializable, DeploymentService {
    
    private static final String CAM_DOMAIN = "com.sun.cam:";
    private static final String PLUGIN_FILTER = "type=plugins,*";
    private static final String TYPE_KEY = "type";
    private static final String NAME_KEY = "name";
    private static final String APPLICATIONS_CONFIG_MBEAN = "com.sun.appserv:type=applications,category=config";
    private static final String FORCE = "force";
    private static final String VIRTUAL_SERVERS = "virtualservers";
    private static final String VERIFY = "verify";
    private static final String ENABLE = "enable";
    private static final String PRECOMPILE_JSP = "precompilejsp";
    private static final String ARCHIVE_NAME = "archiveName";
    private static final String NAME = "name";
    private static final String DESCRIPTION = "description";
    
    /** Creates a new instance of SunManagementService */
    public SunDeploymentService(ServerConnector connector,String targetName) {
        super(connector,targetName);
    }
    
   
       /**
     * installs component ( service engine, binding component)
     * @return lifecycle object name string for service engine or binding component.
     * @param zipFilePath archive file in a zip format
     * @param params Properties Object
     */
    public String installComponent(String zipFilePath, Properties paramProps) {
        String result = "";
        return result;
    }

    /**
     * installs component ( service engine, binding component)
     * @return lifecycle object name string for service engine or binding component.
     * @param zipFilePath archive file in a zip format
     */
    public String installComponent(String zipFilePath) {
        String result = "";
        return result;
    }

    /**
     * uninstalls component ( service engine, binding component)
     * @param componentName name of the component
     */
    public String uninstallComponent(String componentName) {
        String result = "";
        return result;
    }

    /**
     * installs shared namespace
     * @return shared library name object name string for service engine or binding component.
     * @param zipFilePath archive file in a zip format
     */
    public String installSharedLibrary(String zipFilePath) {
        String result = "";
        return result;
    }

    /**
     * uninstalls shared library
     * @param sharedLibraryName name of the shared library
     */
    public String uninstallSharedLibrary(String sharedLibraryName) {
        String result = "";
        return result;
    }


    /**
     * deploys service assembly
     * @return deployment status result as a management message xml
     * @param zipFilePath fie path
     */
    public String deployServiceAssembly(String zipFilePath) {
        String result = "";
        return result;
    }

    /**
     * start service assembly
     * @param serviceAssemblyName name of the service assembly
     */
    public String startServiceAssembly(String serviceAssemblyName) {
        String result = "";
        return result;
    }

    /**
     * stop service assembly
     * @param serviceAssemblyName name of the service assembly
     */
    public String stopServiceAssembly(String serviceAssemblyName) {
        String result = "";
        return result;
    }

    /**
     * shutdown service assembly
     * @param serviceAssemblyName name of the service assembly
     */
    public String shutdownServiceAssembly(String serviceAssemblyName) {
        String result = "";
        return result;
    }


    /**
     * undeploys service assembly
     * @param serviceAssemblyName name of the service assembly
     */
    public String undeployServiceAssembly(String serviceAssemblyName) {
        String result = "";
        return result;
    }    
    
    /**
     * deploy plugin to app server
     * @param archiveFile - local file only
     */
    public String deployPlugin(String archiveFile) {
        String result = "";
        
        try {
            Properties props = prepareDeploymentProperties();
            props.setProperty(ARCHIVE_NAME, archiveFile);
            props.setProperty(NAME, getDefaultComponentName(archiveFile));
            props.setProperty(DESCRIPTION, getDefaultComponentName(archiveFile));
           
            Object[] params = {props};
            String[] sigs = {"java.util.Properties"};
            ObjectName objectName = new ObjectName(APPLICATIONS_CONFIG_MBEAN);
            serverConnection.invoke(objectName, "deploy", params, sigs);
            
        } catch(Exception e) {
            e.printStackTrace();
        }
        
        return result;       
    }
     
    /**
     * undeploy plugin from app server
     * @param pluginName - name of war file to undeploy
     */ 
    public String undeployPlugin(String pluginName) {
        String result = "";
        try {
            Properties props = prepareDeploymentProperties();
            props.setProperty(NAME, pluginName);
            Object[] params = {props};
            String[] sigs = {"java.util.Properties"};
            ObjectName objectName = new ObjectName(APPLICATIONS_CONFIG_MBEAN);
            serverConnection.invoke(objectName, "undeploy", params, sigs);

        } catch(Exception e) {
            e.printStackTrace();
        }
        
        return result;        
    }
    
    
    private Properties prepareDeploymentProperties() {
        Properties props = new Properties();
        //props.setProperty(VIRTUAL_SERVERS, getVirtualServers(connection));
        props.setProperty(FORCE, "true");
        props.setProperty(VERIFY, "false");
        props.setProperty(PRECOMPILE_JSP, "false");
        props.setProperty(ENABLE, "true");        
        return props;
    }
    
    
    public List<String> getPlugins() {
        List<String> plugins = new ArrayList<String>();
        try {
            // create mirrors for remote JMS Servers hosted by app server
            ObjectName remoteObjectNameFilter = new ObjectName(CAM_DOMAIN + PLUGIN_FILTER);
            Set remoteObjNames = serverConnection.queryNames(remoteObjectNameFilter, null);
            if (remoteObjNames != null && remoteObjNames.size() > 0) {
                Iterator iter = remoteObjNames.iterator();
                while (iter.hasNext()) {
                    ObjectName objName = (ObjectName)iter.next();
                    try {
                        // 1. get <name> from objectname
                        String type = objName.getKeyProperty(TYPE_KEY);
                        String name = objName.getKeyProperty(NAME_KEY);
                        plugins.add(name);
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }            
                     
        } catch(Exception e) {
           e.printStackTrace();
        }        
        return plugins;
        
    }
    
    
    private String getDefaultComponentName(String filePath) {
        final String fileName = new File(filePath).getName();
        int toIndex = fileName.lastIndexOf('.');
        if (toIndex < 0) {
            toIndex = fileName.length();
        }
        final String name = fileName.substring(0, toIndex);
        System.out.println("Default Name is: " + name);

        return name;
    }    
    
}
