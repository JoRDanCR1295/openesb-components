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
 * @(#)IEPManagementMbeanRegister.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.iep.admin;

import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;

/**
 *
 * @author IEP
 */
public class IEPAdminMbeanRegister {
    ObjectName objectName;
    MBeanServer connection;
    

    /**
     * Create an instance, providing an MBeanServer to use
     */
    public IEPAdminMbeanRegister(String componentType, String componentName, MBeanServer mbeanServer) throws MalformedObjectNameException {
        this.objectName = constructObjectName(componentType, componentName);
        this.connection = mbeanServer;
    }


    /**
     * Create an instance, providing an MBeanServer to use
     */
    public IEPAdminMbeanRegister(ObjectName objName, MBeanServer mbeanServer) throws MalformedObjectNameException {
        this.objectName = objName;
        this.connection = mbeanServer;
    }   
    /**
     * Register the Runtime Configuration MBean for this component
     * Note that to catch all JMX related exception one can use JMException
     * @param mbeanInstance the object to register, has to be a valid MBean
     */
    public void registerMBean(Object mbeanInstance) throws InstanceAlreadyExistsException, MBeanRegistrationException, NotCompliantMBeanException {
        if(connection != null) {
            // register MBean only if it is not already registered.
            if(this.connection.isRegistered(this.objectName) == false) {
                this.connection.registerMBean(mbeanInstance, this.objectName);
            }
        }
    }

    /**
     * Unregister the runtime configuration MBean for this component
     * Note that to catch all JMX related exception one can use JMException
     */
    public void unregisterMBean() throws InstanceNotFoundException, MBeanRegistrationException {
        if(connection != null) {
            // unregister MBean only if it is not already registered.
            if(this.connection.isRegistered(objectName) == true) {
                this.connection.unregisterMBean(this.objectName);
            }
        }
    }

    /**
     * Get the object name the runtime configuration MBean gets registered as
     */
    public ObjectName getRuntimeConfigObjectName() {
        return objectName;
    }

    /**
     * Constructs the Object Name of the runtime configuration MBean given a component type and name
     * @param componentType type of the component
     * @param componentName name of the component
     * @return the ObjectName using the runtime configuration MBean naming convention
     * @throws MalformedObjectNameException if the resulting object name is invalid
     */
    public static ObjectName constructObjectName(String componentType, String componentName) throws MalformedObjectNameException {
        String objectNameString = "com.sun.ebi:ServiceType=ManagementAPI,InstallationType=" + componentType + ",IdentificationName=" + componentName;
        ObjectName objectName = new ObjectName(objectNameString);
        return objectName;
    }
  
    
}
