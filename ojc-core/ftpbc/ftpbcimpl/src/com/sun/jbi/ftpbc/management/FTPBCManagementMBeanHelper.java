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
package com.sun.jbi.ftpbc.management;

import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;

/**
 * Helper class to handle FTP BC management MBean
 */
public class FTPBCManagementMBeanHelper {

    ObjectName mObjName;
    MBeanServer mMBeanServer;

    /**
     * Create an instance, providing an MBeanServer to use
     */
    public FTPBCManagementMBeanHelper(ObjectName objName, MBeanServer mbeanServer) throws MalformedObjectNameException {
        mObjName = objName;
        mMBeanServer = mbeanServer;
    }

    /**
     * Register the management MBean for the BC
     * Note that to catch all JMX related exception one can use JMException
     * @param mBeanInstance the object to register, has to be a valid MBean
     */
    public void registerMBean(Object mBeanInstance) throws InstanceAlreadyExistsException, MBeanRegistrationException, NotCompliantMBeanException {
        if (mMBeanServer != null) {
            // register MBean only if it is not already registered.
            if (mMBeanServer.isRegistered(mObjName) == false) {
                mMBeanServer.registerMBean(mBeanInstance, mObjName);
            }
        }
    }

    /**
     * Unregister the management MBean for the BC
     * Note that to catch all JMX related exception one can use JMException
     */
    public void unregisterMBean() throws InstanceNotFoundException, MBeanRegistrationException {
        if (mMBeanServer != null) {
            // unregister MBean only if it is not already registered.
            if (mMBeanServer.isRegistered(mObjName) == true) {
                mMBeanServer.unregisterMBean(mObjName);
            }
        }
    }

    /**
     * Utility method to retrieve the management MBean object name
     */
    public ObjectName getManagementMBeanObjectName() {
        return mObjName;
    }
}
