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
 * @(#)PluginsManagerBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.plugins;

import com.sun.data.provider.RowKey;
import com.sun.data.provider.TableDataProvider;
import com.sun.data.provider.impl.ObjectListDataProvider;
import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.manager.framework.common.BaseBean;
import com.sun.jbi.cam.manager.framework.common.FacesUtil;
import com.sun.jbi.cam.manager.framework.core.ComponentServiceProviderResolver;
import com.sun.jbi.cam.services.deployment.DeploymentService;
import java.io.File;
import java.io.Serializable;

import com.sun.webui.jsf.component.Upload;
import com.sun.webui.jsf.model.UploadedFile;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;
import javax.faces.event.ValueChangeEvent;
import javax.faces.component.UIInput;
import com.sun.jbi.cam.services.ServiceManager;
import com.sun.jbi.cam.services.administration.AdministrationService;
import com.sun.jbi.cam.services.ServiceManagerFactory;

/**
 * @todo - refactor when upload tag is fixed
 *
 * @author ylee
 */
public class PluginsManagerBean extends BaseBean implements Serializable {

     //
     // Holds value of property uploadedFile.
     //
    private UploadedFile uploadedFile;
    private String fileName;
    private String targetName;
    private Logger logger = Logger.getLogger(PluginsManagerBean.class.getName());
    private final String ZIP_SUFFIX = ".zip";
    private final String WAR_SUFFIX = ".war";
    private final String JAR_SUFFIX = ".jar";
    
    private DeploymentService deployService;
 
    
    /** Creates a new instance of PluginManager */
    public PluginsManagerBean() {
    }

     //
     // Getter for property uploadedFile.
     // @return Value of property uploadedFile.
     //
    public UploadedFile getUploadedFile() {
        return this.uploadedFile;
    }

     //
     // Setter for property uploadedFile.
     // @param uploadedFile New value of property uploadedFile.
     //
    public void setUploadedFile(UploadedFile uploadedFile) {
        System.out.println(">>>> setUploadedFile called:" + uploadedFile);
        this.uploadedFile = uploadedFile;
        if (uploadedFile!=null ) {
            System.out.println(">>>> setUploadedFile called: uploaded filename - "+uploadedFile.getOriginalName());
        }
    }
    
    
    public String getFileName() {
        if ( logger.isLoggable(Level.INFO) ) {
            logger.info("get fileName: "+fileName);
        }
        return fileName;
    }
    
    
    public void setFileName(String name) {
        if ( logger.isLoggable(Level.INFO ) ) {
            logger.info("set fileName: "+name);
        }
        this.fileName = name;
    }
    
    public String getTargetName() {
        if ( logger.isLoggable(Level.INFO) ) {
            logger.info("get targetName: "+targetName);
        }
        return targetName;
    }
    
    
    public void setTargetName(String name) {
        if ( logger.isLoggable(Level.INFO ) ) {
            logger.info("set targetName: "+name);
        }
        this.targetName = name;
    }
    
    public void actionListener(ActionEvent e) {
        // 
        FacesContext context = FacesContext.getCurrentInstance();
        System.out.println(">>>> actionListener called: "+ e.getComponent().getClientId(context));
        System.out.println(">>>> actionListener called: filename - "+fileName);
        System.out.println(">>>> actionListener called: uploadedFile - "+uploadedFile);
        if (uploadedFile!=null ) {
            System.out.println(">>>> actionListener called: uploaded filename - "+uploadedFile.getOriginalName());
        }
    }
     
    /** Action handler for all buttons. */
    public String actionHandler() {
        // Returning null causes page to re-render.
        // 
        FacesContext context = FacesContext.getCurrentInstance();
        
        System.out.println(">>>> actionHandler called: ");
        if (uploadedFile!=null ) {
            System.out.println(">>>> actionHandler called: uploaded filename - "+uploadedFile.getOriginalName());
        }
        
        deployComponent();
        
        return "";
    }
    

    public void valueChangeHandler(ValueChangeEvent event) {
        
        Object newValue = event.getNewValue();
        System.out.println(">>>> newValue: "+newValue);
        
    }    
    
    
    private void deployComponent() {
        System.out.println(">>>>> DeployComponent: fileName: "+fileName+" targetName; "+targetName);
        String status = "";
        ServiceManager mgr = ServiceManagerFactory.getServiceManager(componentName,componentType);
        AdministrationService adminService = mgr.getAdministrationService(targetName);   
        if ( fileName!=null ) {
            if ( fileName.endsWith(ZIP_SUFFIX) ) {
                // deploy SA
                if ( logger.isLoggable(Level.INFO ) ) {
                    logger.info("deploying service assembly: "+fileName);
                }
                status = adminService.deployServiceAssembly(fileName);
                                
            } else if ( fileName.endsWith(JAR_SUFFIX )) {
                // deploy JBI component (SE/BC)
                if ( logger.isLoggable(Level.INFO ) ) {
                    logger.info("deploying component: "+fileName);
                }
                status = adminService.installComponent(fileName);                
            } else {
                // deploy WAR
                if ( logger.isLoggable(Level.INFO ) ) {
                    logger.info("deploying war: "+fileName);
                }                
            }
        }
        if ( logger.isLoggable(Level.INFO ) ) {
            logger.info("deployed status: "+status);
        }
        
    }
    
    public TableDataProvider getMappings() {
        
        List<DisplayMappings> list = new ArrayList<DisplayMappings>();
        
        // get SP mappings from ComponentServiceProviderResolver
        ComponentServiceProviderResolver resolver = ComponentServiceProviderResolver.getInstance();
        Map<String,String> mappings = resolver.getMappings();
        // generate display mappings
        for ( Iterator iter=mappings.entrySet().iterator(); iter.hasNext(); ) {
           Map.Entry entry = (Map.Entry)iter.next();
           String key = (String)entry.getKey();
           String value = (String)entry.getValue();
           DisplayMappings displayMapping = new DisplayMappings(key,value);
           list.add(displayMapping);
        }
        
        provider = new ObjectListDataProvider(list);
        
        return provider;        
    }
    
    
    
    public TableDataProvider getPlugins() {
        
        List<DisplayPlugin> list = new ArrayList<DisplayPlugin>();
        
        ServiceManager mgr = ServiceManagerFactory.getServiceManager(componentName,componentType);
        deployService = mgr.getDeploymentService(GenericConstants.ADMIN_SERVER);
        
        List<String> pluginList = deployService.getPlugins();
        if ( pluginList!=null ) {
            for ( Iterator iter=pluginList.iterator(); iter.hasNext(); ) {
                String pluginName = (String)iter.next();
                DisplayPlugin plugin = new DisplayPlugin(pluginName);
                list.add(plugin);
            }
        }
      
        provider = new ObjectListDataProvider(list);
        
        return provider;        
    }
        
    
    public void setPlugins(TableDataProvider provider) {
        this.provider = provider;
    }
    
    public String undeploy() {
        System.out.println("undeploy...");
        if ( provider!=null ) {
            RowKey rowKey = FacesUtil.getTableRow("plugins");
            String pluginName = (String)provider.getValue(provider.getFieldKey("name"),rowKey);
            System.out.println("pluginName: "+pluginName);
            deployService.undeployPlugin(pluginName);
        }
        return "";
    }
    
    public String deploy() {
        if (uploadedFile!=null ) {
            System.out.println(">>>> deploy called: uploaded filename - "+uploadedFile.getOriginalName());
            fileName = uploadedFile.getOriginalName();
        }   
        System.out.println("deploy..."+fileName);
        if ( fileName!=null ) {
            // save file to temp folder
            String outputFileName = saveFile();
            // deploy it
            deployService.deployPlugin(outputFileName);
            fileName = null;
        }
        return "";
    }
    
    
    private String saveFile() {
        
        String outputFileName = null;
        String rootPath = System.getProperty("java.io.tmpdir");
        Date date = new Date();
        String deployPath = File.separator+"cam"+File.separator+"deploy"+File.separator+File.separator+date.getTime();
        File deployFolder = new File(rootPath+File.separator+deployPath);
        if (deployFolder.exists() == false) {
             try {
                deployFolder.mkdirs();
            } catch (RuntimeException e) {
                logger.log(Level.SEVERE,e.getMessage(),e);
            }
        }
        
        try {
            File outputFile = new File(rootPath+File.separator+deployPath+File.separator+fileName);     
            outputFileName = outputFile.getCanonicalPath();
            System.out.println("Saving "+outputFileName);
            uploadedFile.write(outputFile);
        } catch(Exception e) {
            e.printStackTrace();
        }
        
        return outputFileName;
    }
    
}
