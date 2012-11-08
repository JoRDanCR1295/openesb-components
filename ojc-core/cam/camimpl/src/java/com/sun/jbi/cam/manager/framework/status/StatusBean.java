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
 * @(#)StatusBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.status;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.manager.framework.common.BaseBean;
import com.sun.jbi.cam.model.management.JBIServiceAssemblyStatus;
import com.sun.jbi.cam.model.management.JBIServiceUnitStatus;
import com.sun.jbi.cam.services.administration.AdministrationService;
import com.sun.jbi.cam.services.management.ManagementService;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * @todo
 *   a) implement refresh interval
 *
 * @author ylee
 */
public class StatusBean extends BaseBean implements Serializable {
    
    private static StatusBean statusInstance = null;
    
    private Map<String,String> statusMap = new TreeMap<String,String>();
    
    private ManagementService managementService;
    private AdministrationService adminService;
    private List<JBIServiceAssemblyStatus> saList;
    
    /** Creates a new instance of StatusBoard */
    private StatusBean() {
          setup();
    }

    public static StatusBean getInstance() {
        if ( statusInstance==null ) {
            statusInstance = new StatusBean();
        }
        return statusInstance;
    }
    
    public String getStatus(String componentName, String componentType, String targetName, String saName) {
        // construct key to retrieve status from statusMap
        String key = constructKey(componentName,componentType,targetName,saName);
        String status =  statusMap.get(key);
        //System.out.println(">>> getting component status - key: "+key+" status:"+status);
        return status;
    }

    public String getStatus(String key) {
        // construct key to retrieve status from statusMap
        String status =  statusMap.get(key);
        //System.out.println(">>> getting component status - key: "+key+" status:"+status);
        return status;
    }
    
    
    public void setStatus(String componentName, String componentType, String targetName, String saName, String status) {
        if ( status!=null ) {
            String key = constructKey(componentName,componentType,targetName,saName);
            //System.out.println(">>> setting component status - key: "+key+" status:"+status);
            statusMap.put(key,status);
        }
    }
    
    public void setStatus(String key, String status) {
        if ( status!=null ) {
            //System.out.println(">>> setting component status - key: "+key+" status:"+status);
            statusMap.put(key,status);
        }
    }    
    
    public void removeStatus(String componentName, String componentType, String targetName, String saName) {
        String key = constructKey(componentName,componentType,targetName,saName);
        //System.out.println(">>> removing component status - key: "+key);
        statusMap.remove(key);
    }

    public void removeStatus(String key) {
        //System.out.println(">>> removing component status - key: "+key);
        statusMap.remove(key);
    }
    
    private String constructKey(String componentName, String componentType, String targetName,String saName) {
        String key =  targetName + ":" + componentName + ":" + componentType + ":" + saName;
        return key;
    }
    
    private Map<String,String> splitKey(String key) {
        Map<String,String> keyMap = new HashMap<String,String>();
        String[] parts = key.split(":");
        keyMap.put(GenericConstants.COMPONENT_TNAME,parts[0]);
        keyMap.put(GenericConstants.COMPONENT_NAME,parts[1]);
        keyMap.put(GenericConstants.COMPONENT_TYPE,parts[2]);
        keyMap.put(GenericConstants.COMPONENT_PNAME,parts[3]);
        return keyMap;
    }
    
    public void refresh() {
        
        //  walkthru the status map... 
        String targetName = "";
        for (Iterator iter=statusMap.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry)iter.next();
            String key = (String)entry.getKey();
            String value = (String)entry.getValue();
            Map<String,String> keyMap = splitKey(key);
            
            // setup parameters
            tName = keyMap.get(GenericConstants.COMPONENT_TNAME);
            componentName = keyMap.get(GenericConstants.COMPONENT_NAME);
            componentType = keyMap.get(GenericConstants.COMPONENT_TYPE);
            pName = keyMap.get(GenericConstants.COMPONENT_PNAME);
            
            // get new services if target is different
            if ( targetName.equalsIgnoreCase(tName)==false ) {
                getServices();
                targetName = tName;
            }
            
            String status = GenericConstants.UNKNOWN_STATE;
            if ( componentType.equalsIgnoreCase(GenericConstants.BC_TYPE) || 
                componentType.equalsIgnoreCase(GenericConstants.SE_TYPE) ) {
                status = managementService.getState(componentName,componentType);
            } else if ( componentType.equalsIgnoreCase(GenericConstants.SA_TYPE) ) {
                status = getSaStatus(componentName);
            } else if ( componentType.equalsIgnoreCase(GenericConstants.SU_TYPE) ) {
                // SU case
                status = getSuStatus(componentName,pName);
            }
            
            if ( value.equalsIgnoreCase(status)==false ) {
                // update statusboard
                setStatus(key,status);
            }   
        }
        
    }    
    
    
    private String getSaStatus(String componentName) {
        String status = GenericConstants.UNKNOWN_STATE;
        Iterator<JBIServiceAssemblyStatus> saIter = saList.iterator();
        while( saIter.hasNext()) {
             JBIServiceAssemblyStatus saStatus = saIter.next();
             String saName = saStatus.getServiceAssemblyName();
             if ( componentName.equals(saName) ) {
                status = saStatus.getStatus();
                break;
             }
        }
        return status;
    }
    
    
    private String getSuStatus(String componentName,String saName) {
        String status = GenericConstants.UNKNOWN_STATE;
        Iterator<JBIServiceAssemblyStatus> saIter = saList.iterator();
        while( saIter.hasNext() ) {
            JBIServiceAssemblyStatus saStatus = saIter.next();
            String serviceAssemblyName = saStatus.getServiceAssemblyName();
            if (saName.equals(serviceAssemblyName)) {
               List<JBIServiceUnitStatus> suList = saStatus.getJbiServiceUnitStatusList();
               // find SU
               for (Iterator suIter=suList.iterator(); suIter.hasNext(); ) {
                   JBIServiceUnitStatus suStatus = (JBIServiceUnitStatus)suIter.next();
                   if ( suStatus.getServiceUnitName().equals(componentName) ) {
                       status = suStatus.getStatus();
                   }
               }
            }
        }        
        return status;
    }
    

    private void getServices() {
        getServiceManager();
        managementService = serviceManager.getManagementService(tName);
        adminService = serviceManager.getAdministrationService(tName);
        saList = adminService.getServiceAssemblyList();        
    }
    
   
    public void clear() {
        //System.out.println("clearing status board...");
        statusMap.clear();
    }
    
}
