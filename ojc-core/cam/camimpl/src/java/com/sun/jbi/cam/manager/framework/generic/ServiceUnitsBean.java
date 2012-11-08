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
 * @(#)ServiceUnitsBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.generic;

import com.sun.data.provider.TableDataProvider;
import com.sun.data.provider.impl.ObjectListDataProvider;
import com.sun.enterprise.admin.common.Name;
import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.Util;
import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.manager.framework.common.BaseBean;
import com.sun.jbi.cam.model.management.JBIServiceAssemblyStatus;
import com.sun.jbi.cam.model.management.JBIServiceUnitStatus;
import com.sun.jbi.cam.services.administration.AdministrationService;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

/**
 *
 * @author ylee
 */
public class ServiceUnitsBean extends BaseBean implements Serializable {
    
    protected AdministrationService adminService;
    private transient Logger logger = Logger.getLogger(ServiceUnitsBean.class.getName());
    
    /** Creates a new instance of ServiceUnitsBean */
    public ServiceUnitsBean() {
        setup();
    }
    
    
    public TableDataProvider getList() {
        List<DisplayServiceUnits> list = getServiceUnitList(null);
        provider = new ObjectListDataProvider(list);
        
        return provider;
    }
     
        
    public String getTitle() {
        return getName()+" - " + Messages.getString("serviceUnits_title");
    }
    
    public String getTableTitle() {
        return getTableTitle("serviceUnits_bc_tabletitle","serviceUnits_se_tabletitle","serviceUnits_su_tabletitle");
    }    
    

    public List<DisplayServiceUnits> getServiceUnitList(String serviceAssemblyName) {
                // setup request configuration data
        setup();
 
        List<DisplayServiceUnits> list = new ArrayList<DisplayServiceUnits>();
        
        List<JBIServiceAssemblyStatus> saList = getSAList(tName);
        Iterator<JBIServiceAssemblyStatus> saIter = saList.iterator();
        while( saIter.hasNext()) {
            JBIServiceAssemblyStatus saStatus = saIter.next();
            String saName = saStatus.getServiceAssemblyName();
             Iterator<JBIServiceUnitStatus> suIter = saStatus.getJbiServiceUnitStatusList().iterator();
            while( suIter.hasNext() ) {
                JBIServiceUnitStatus suStatus = suIter.next();
                String target = suStatus.getTargetName();
                // add DisplayServiceUnits on component match
                // or if the component is service assembly that matches
                // the service assembly provided has parameter to this method
                if ( target.equals(componentName) || 
                       (componentName.equals(serviceAssemblyName) &&
                        componentType.equals(GenericConstants.SA_TYPE))) {
                    String name = suStatus.getServiceUnitName();
                    String desc = suStatus.getServiceUnitDescription();
                    String url = getProviderUrl(Util.fixupName(name,saName),
                                                GenericConstants.SU_TYPE,
                                                componentName,
                                                componentType)
                        +"?"+GenericConstants.COMPONENT_NAME+"="+Util.fixupName(name,saName)+"&"
                        +GenericConstants.COMPONENT_TYPE+"="+GenericConstants.SU_TYPE+"&"
                        +GenericConstants.COMPONENT_CNAME+"="+componentName+"&"
                        +GenericConstants.COMPONENT_CTYPE+"="+componentType;
                    String status = suStatus.getStatus();
                    DisplayServiceUnits su = new DisplayServiceUnits(name,desc,url,status);
                    list.add(su);
                }
            }
        }
        return list;
    }

    public List<JBIServiceAssemblyStatus>  getSAList(String targetName) {
        if ( targetName==null ) {
            logger.severe(Messages.getString("SU.target.name.error"));
            //return new ArrayList<JBIServiceAssemblyStatus>();
            // force exception stacktrace
            return null;
        }
        adminService = serviceManager.getAdministrationService(targetName);
        //adminService.prepare(targetName);
        return  adminService.getServiceAssemblyList();
    }
    
    public List<JBIServiceUnitStatus> getSAServiceUnitStatusList(String saName, String targetName){
        
        Iterator<JBIServiceAssemblyStatus> saIter = getSAList(targetName).iterator();
        while( saIter.hasNext() ) {
            JBIServiceAssemblyStatus saStatus = saIter.next();
            String serviceAssemblyName = saStatus.getServiceAssemblyName();
            if(saName.equals(serviceAssemblyName)) {
               List<DisplayServiceUnits> list = 
                       new ArrayList<DisplayServiceUnits>();
               List<JBIServiceUnitStatus> suList = 
                       saStatus.getJbiServiceUnitStatusList();
               
               return suList;
            }
            
        }
        // empty list
        return new ArrayList<JBIServiceUnitStatus>();
    }
    
    public String getServiceAsseemblyState(String saName, String targetName){
        List<JBIServiceAssemblyStatus> saList = getSAList(targetName);
        int count = saList.size();
        for(int index=0; index <count; index++) {
            JBIServiceAssemblyStatus saStatus = 
                   (JBIServiceAssemblyStatus)saList.get(index);
            if(saStatus.getServiceAssemblyName().equals(saName)) {
                return saStatus.getStatus();
            }
        }
        return Messages.getString("state.Unavailable");
    }

    /**
     * getProviderUrl - retrieves service provider URL
     * @param   componentType    -  
     */
    public String getProviderUrl(String componentType) {
        if ( adminService==null ) {
             adminService = serviceManager.getAdministrationService(tName);
        }
        return adminService.getProviderUrl(componentType);
    }
    
    /**
     * getProviderUrl - retrieves service provider URL
     * @param   name    -   name of subcomponent 
     * @param   type    -   type of subcomponent 
     * @param   componentName   -      
     * @param   componentType   -   
     */
    public String getProviderUrl(String name,String type, String componentName,String componentType) {
        if ( adminService==null ) {
             adminService = serviceManager.getAdministrationService(tName);
        }
        return adminService.getProviderUrl(name,type,componentName,componentType);
    }    

    
}
