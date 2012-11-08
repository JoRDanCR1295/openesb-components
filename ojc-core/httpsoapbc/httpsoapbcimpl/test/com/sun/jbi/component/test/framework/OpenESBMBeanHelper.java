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
 * @(#)OpenESBIntegrationTestBase.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.test.framework;

import java.util.Properties;

import javax.management.Attribute;
import javax.management.ObjectName;

import com.sun.jbi.ui.client.JMXConnection;
import com.sun.jbi.ui.client.JMXConnectionFactory;

/**
 *
 * Utility class for OpenESB Component Container MBeans
 */
public class OpenESBMBeanHelper {

    static final String STATUS_OBJECT_NAME_PREFIX = "com.sun.ebi:ServiceType=Status,InstallationType=";
    static final String RUNTIME_CONFIG_OBJECT_NAME_PREFIX = "com.sun.ebi:ServiceType=Configuration,InstallationType=";
    static final String OBJECT_NAME_SUFFIX = ",IdentificationName=";

    static final String COMPONENT_TYPE_BINDING = "Binding";
    static final String COMPONENT_TYPE_ENGINE = "Engine";
    
    // constants for the new status MBean name construction
    // note this does NOT cover clusters
    static final String NEW_STATUS_MBEAN_OBJECT_NAME_PREFIX = "com.sun.jbi:JbiName=server,CustomControlName=Statistics,ComponentName="; 
    static final String NEW_STATUS_MBEAN_OBJECT_NAME_SUFFIX = ",ControlType=Custom,ComponentType=Installed,InstalledType=";
    
    public static ObjectName getBindingStatusProviderMBeanName(String componentName) throws Exception {
        return constructObjectName(STATUS_OBJECT_NAME_PREFIX, COMPONENT_TYPE_BINDING, componentName);
    }

    public static ObjectName getEngineStatusProviderMBeanName(String componentName) throws Exception {
        return constructObjectName(STATUS_OBJECT_NAME_PREFIX, COMPONENT_TYPE_ENGINE, componentName);
    }

    public static ObjectName getBindingRuntimeConfigurationMBeanName(String componentName) throws Exception {
        return constructObjectName(RUNTIME_CONFIG_OBJECT_NAME_PREFIX, COMPONENT_TYPE_BINDING, componentName);
    }

    public static ObjectName getEngineRuntimeConfigurationMBeanName(String componentName) throws Exception {
        return constructObjectName(RUNTIME_CONFIG_OBJECT_NAME_PREFIX, COMPONENT_TYPE_ENGINE, componentName);
    }
    
    /** Use this method to construct runtime configuration MBean name ONLY 
      * For binding components, use getBindingStatusMBeanObjectName to retrieve the correct Status MBean name 
      */
    private static ObjectName constructObjectName(String prefix, String componentType, String componentName) throws Exception {
        ObjectName objectName = null;
        String objectNameString = null;
        String type = null;
        if(componentType != null) {
            if(componentType.equals("Binding") ||
                    componentType.equals("bindingComponents")) {
                type = "bindingComponents";
            } else
                if(componentType.equals("Engine") ||
                    componentType.equals("engineComponents")) {
                type = "serviceEngines";
                } else
                    if(componentType.equals("Namespace") ||
                    componentType.equals("sharedLibraries")) {
                type = "sharedLibraries";
                    } else
                        if(componentType.equals("serviceAssemblies") ||
                    componentType.equals("Deployment")) {
                type = "serviceAssemblies";
                        }
        }
        if((type != null) &&
                (componentName != null)) {
            objectNameString = prefix +
                    type+
                    OBJECT_NAME_SUFFIX+
                    componentName;
            objectName = new ObjectName(objectNameString);
        }
        return objectName;
    }
 
    public static ObjectName getBindingStatusMBeanObjectName(String componentName) throws Exception {
        return constructStatusMBeanObjectName(COMPONENT_TYPE_BINDING, componentName);
    }
    
    private static ObjectName getEngineStatusMBeanObjectName(String componentType, String componentName) throws Exception {
        return constructStatusMBeanObjectName(COMPONENT_TYPE_ENGINE, componentName);
    }
    
    public static ObjectName constructStatusMBeanObjectName(String componentType, String componentName) throws Exception {
        ObjectName objectName = null;
        String objectNameString = null;
        String type = null;
        boolean isBindingComponent = true;
        
        if(componentType != null) {
            if(componentType.equalsIgnoreCase("Binding") ||
               componentType.equalsIgnoreCase("bindingComponents")) {
               type = "bindingComponents";
            } else if(componentType.equalsIgnoreCase("Engine") ||
                      componentType.equalsIgnoreCase("engineComponents")) {
                isBindingComponent = false;
                type = "serviceEngines";
            } 
        }
        
        if((type != null) && (componentName != null)) {
            objectNameString = NEW_STATUS_MBEAN_OBJECT_NAME_PREFIX +
                               componentName +
                               NEW_STATUS_MBEAN_OBJECT_NAME_SUFFIX +
                               (isBindingComponent? COMPONENT_TYPE_BINDING : COMPONENT_TYPE_ENGINE);
            objectName = new ObjectName(objectNameString);
        }
        
        return objectName;
    }

    public static Object invokeMBeanOperation(ObjectName objectname, String s, Object aobj[], String as[], Properties connectionProps)
        throws Exception {
        JMXConnection connection = JMXConnectionFactory.newInstance().getConnection(connectionProps);
        connection.openConnection();
        Object result = connection.invokeMBeanOperation(objectname, s, aobj, as);
        connection.closeConnection();
        return result;
    }

    public static Object getMBeanAttribute(ObjectName objectname, String s, Properties connectionProps)
        throws Exception {
        JMXConnection connection = JMXConnectionFactory.newInstance().getConnection(connectionProps);
        connection.openConnection();
        Object result = connection.getMBeanAttribute(objectname, s);
        connection.closeConnection();
        return result;
    }

    public static void setMBeanAttribute(ObjectName objectname, Attribute attribute, Properties connectionProps)
        throws Exception {
        JMXConnection connection = JMXConnectionFactory.newInstance().getConnection(connectionProps);
        connection.openConnection();
        connection.setMBeanAttribute(objectname, attribute);
        connection.closeConnection();
    }
}
