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
 * @(#)RuntimeConfigurationHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.config;

import javax.jbi.component.ComponentContext;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;

import com.sun.jbi.crl.util.Util;

/**
 * Utility to help with Runtime Configuration
 * @author Sun Microsystems
 */
public class RuntimeConfigurationHelper {
    public static final String COMPONENT_TYPE_BINDING = "bindingComponents";
    public static final String COMPONENT_TYPE_ENGINE = "serviceEngines";

    private ObjectName mObjectName;
    private MBeanServer mConnection;

    public RuntimeConfigurationHelper(ComponentContext ctx) {
        this(ctx.getMBeanNames().createCustomComponentMBeanName("Configuration"), 
             ctx.getMBeanServer());
    }
    
    public RuntimeConfigurationHelper(ObjectName objectName, 
    								  MBeanServer mbeanServer) {
        mObjectName = objectName;
        mConnection = mbeanServer;
    }

    /**
     * Register the Runtime Configuration MBean for this component
     * Note that to catch all JMX related exception one can use JMException
     * @param mbeanInstance the object to register, has to be a valid MBean
     */
    public void registerMBean(Object mbeanInstance) throws InstanceAlreadyExistsException, 
    													   MBeanRegistrationException, 
    													   NotCompliantMBeanException {
        if (mConnection != null) {
            // register MBean only if it is not already registered.
            if (!mConnection.isRegistered(mObjectName)) {
                mConnection.registerMBean(mbeanInstance, mObjectName);
            }
        }
    }

    /**
     * Unregister the runtime configuration MBean for this component
     * Note that to catch all JMX related exception one can use JMException
     */
    public void unregisterMBean() throws InstanceNotFoundException, MBeanRegistrationException {
        if(mConnection != null) {
            // unregister MBean only if it is not already registered.
            if (mConnection.isRegistered(mObjectName)) {
                mConnection.unregisterMBean(mObjectName);
            }
        }
    }

    /**
     * Get the object name the runtime configuration MBean gets registered as
     */
    public ObjectName getRuntimeConfigObjectName() {
        return mObjectName;
    }

    /**
     * Constructs the Object Name of the runtime configuration MBean given a component type and name
     * @param componentType type of the component
     * @param componentName name of the component
     * @return the ObjectName using the runtime configuration MBean naming convention
     * @throws MalformedObjectNameException if the resulting object name is invalid
     */
    public static ObjectName constructObjectName(String componentType, String componentName) throws MalformedObjectNameException {
        return constructObjectName(componentType, componentName, null);
    }

    /**
     * Constructs the Object Name of the runtime configuration MBean given a 
     * component type,name and the serviceAssemblyName using the component
     * @param componentType type of the component
     * @param componentName name of the component
     * @param serviceAssembly name the component is used in.
     * @return the ObjectName using the runtime configuration MBean naming convention
     * @throws MalformedObjectNameException if the resulting object name is invalid
     */
    public static ObjectName constructObjectName(String componentType, String componentName, 
                                                 String serviceUnitId) throws MalformedObjectNameException {
        String objectNameString = "com.sun.ebi:ServiceType=Configuration,InstallationType=" +
                componentType + ",IdentificationName=" + componentName;
        if (Util.isEmpty(serviceUnitId)) {
            objectNameString += ",ServiceUnitID=" + serviceUnitId;
        }
        
        return new ObjectName(objectNameString);
    }
}
