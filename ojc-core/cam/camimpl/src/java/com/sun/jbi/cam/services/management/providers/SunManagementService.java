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
 * @(#)SunManagementService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.services.management.providers;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.Util;
import com.sun.jbi.cam.connectors.ServerConnector;
import com.sun.jbi.cam.services.BaseServiceProvider;
import com.sun.jbi.cam.services.management.ManagementService;
import com.sun.jbi.cam.services.management.StateConstants;
import java.io.Serializable;
import java.util.Properties;
import java.util.logging.Logger;
import javax.management.ObjectName;

/**
 *
 * @author ylee
 */
public class SunManagementService extends BaseServiceProvider implements Serializable, ManagementService {
    
    private Logger logger = Logger.getLogger(SunManagementService.class.getName());
    
    private static final String DOMAIN_NAME_PREFIX = "com.sun.jbi:";                    //$NON-NLS-1$
    private static final String DOMAIN_NAME_SUFFIX = "JbiName=";                        //$NON-NLS-1$
    private static final String CONTROL_TYPE_PREFIX="ControlType=Lifecycle";            //$NON-NLS-1$
    private static final String INSTALLED_TYPE_PREFIX = "InstalledType=";               //$NON-NLS-1$
    private static final String COMPONENT_TYPE_PREFIX = "ComponentType=Installed";      //$NON-NLS-1$
    private static final String COMPONENT_NAME_PREFIX = "ComponentName=";               //$NON-NLS-1$
    private static final String ADMIN_SERVICE_OBJECTNAME = "com.sun.jbi:ServiceName=JbiReferenceAdminUiService,ComponentType=System";
    
    
    /** Creates a new instance of SunManagementService */
    public SunManagementService(ServerConnector connector,String targetName) {
        super(connector,targetName);
    }
    
    
    /**
     * get the state of the component
     */
    public String getState(String componentName, String componentType) {
        String state = StateConstants.SHUTDOWN_STATE;
        
        try {
            String name = getObjectName(componentName,componentType);
            ObjectName objName = new ObjectName(name);
            // get attribute "CurrentState"
            state = (String)getAttribute(objName,"CurrentState");     //$NON-NLS-1$
            
        } catch(Exception e) {
            e.printStackTrace();
        }
        return state;
    }
    
    private String getObjectName(String componentName, String componentType) {
        String name =
                    DOMAIN_NAME_PREFIX +
                    DOMAIN_NAME_SUFFIX  + targetName + GenericConstants.COMMA_SEPARATOR +
                    COMPONENT_NAME_PREFIX + componentName + GenericConstants.COMMA_SEPARATOR + 
                    CONTROL_TYPE_PREFIX + GenericConstants.COMMA_SEPARATOR +
                    COMPONENT_TYPE_PREFIX +  GenericConstants.COMMA_SEPARATOR +
                    INSTALLED_TYPE_PREFIX + Util.mapInstalledType(componentType);
        return name;
    }
    

    /**
     * start (service engine, binding component,service assembly, service unit, endpoint)
     * @param componentName name of the component
     */
    public String start(String componentName, String componentType) {
        String result = "";
        if(componentType.equals(GenericConstants.SA_TYPE)) {
            result = startServiceAssembly(componentName);
        } else {
            try {
                String name = getObjectName(componentName,componentType);
                ObjectName objName = new ObjectName(name);
                invoke(objName,"start");     //$NON-NLS-1$
            } catch(Exception e) {
                e.printStackTrace();
            }       
        }
        return result;
    }

    /**
     * stop (service engine, binding component,service assembly, service unit, endpoint)
     * @param componentName name of the component
     */
    public String stop(String componentName, String componentType) {
        String result = "";
        if(componentType.equals(GenericConstants.SA_TYPE)) {
            result = stopServiceAssembly(componentName);
        } else {
            try {
                String name = getObjectName(componentName,componentType);
                ObjectName objName = new ObjectName(name);
                invoke(objName,"stop");     //$NON-NLS-1$
            } catch(Exception e) {
                e.printStackTrace();
            }       
        }
        return result;
    }

    /**
     * shutdowm (service engine, binding component,service assembly, service unit, endpoint)
     * @param componentName name of the component
     */
    public String shutdown(String componentName, String componentType) {
        String result = "";
        if(componentType.equals(GenericConstants.SA_TYPE)) {
            result = shutdownServiceAssembly(componentName);
        } else {
            try {
                String name = getObjectName(componentName,componentType);
                ObjectName objName = new ObjectName(name);
                invoke(objName,"shutDown");     //$NON-NLS-1$
            } catch(Exception e) {
                e.printStackTrace();
            }       
        }
        return result;
    }
    
    /**
     * shutdowmForce (service engine, binding component,service assembly, service unit, endpoint)
     * @param componentName name of the component
     */
    public String shutdownForce(String componentName, String componentType) {
        String result = "";
        try {
            String name = getObjectName(componentName,componentType);
            ObjectName objName = new ObjectName(name);
            invoke(objName,"shutDownForce");     //$NON-NLS-1$
        } catch(Exception e) {
            e.printStackTrace();
        }        
        return result;
    }    
    
    public String suspend(String componentName, String componentType) {
        String result = "";
        // todo
        return result;
    }

    public String resume(String componentName, String componentType) {
        String result = "";
        // todo
        return result;
    }
    
    private String startServiceAssembly(String componentName) {
        String result = "";
        try {
            ObjectName objName = new ObjectName(ADMIN_SERVICE_OBJECTNAME);
            invoke(objName,"startServiceAssembly",new Object[] {componentName,targetName});     //$NON-NLS-1$
        } catch(Exception e) {
            e.printStackTrace();
        }        
        return result;
    }
    
    private String stopServiceAssembly(String componentName) {
        String result = "";
        try {
            ObjectName objName = new ObjectName(ADMIN_SERVICE_OBJECTNAME);
            invoke(objName,"stopServiceAssembly",new Object[] {componentName,targetName});     //$NON-NLS-1$
        } catch(Exception e) {
            e.printStackTrace();
        }        
        return result;
    }
     
    private String shutdownServiceAssembly(String componentName) {
        String result = "";
        try {
            ObjectName objName = new ObjectName(ADMIN_SERVICE_OBJECTNAME);
            invoke(objName,"shutdownServiceAssembly",new Object[] {componentName,targetName});     //$NON-NLS-1$
        } catch(Exception e) {
            e.printStackTrace();
        }        
        return result;
    }
 
}
