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
 * @(#)Sample.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.sample;

import java.util.logging.Logger;
import javax.management.MBeanRegistration;
import javax.management.MBeanServer;
import javax.management.ObjectName;

/**
 *
 * @author ylee
 */
public class Sample implements SampleMBean, MBeanRegistration {
    
    private MBeanServer mbeanServer;
    
    private Logger logger = Logger.getLogger(Sample.class.getName());
    
    private static String CAM_SERVER_MBEAN_NAME = "com.sun.cam:type=server,name=ServerMBean";
    
    
    /** Creates a new instance of SampleMBeanImpl */
    public Sample() {
    }

    public void registerServiceProvider() {
        if ( mbeanServer!=null ) {
            try {
                ObjectName objName = new ObjectName(CAM_SERVER_MBEAN_NAME);
                if ( mbeanServer.isRegistered(objName) ) {
                    // register service provider
                    Object[] params = {"com.sun.etlse-1.0-2:SE","http://www.yahoo.com"};
                    String[] sigs = {"java.lang.String","java.lang.String"};
                    mbeanServer.invoke(objName,"registerServiceProvider",params,sigs);
                }
            } catch(Exception e) {
                e.printStackTrace();
            }
        }
    }
    
    public void unregisterServiceProvider() {
        if ( mbeanServer!=null ) {
            try {
                ObjectName objName = new ObjectName(CAM_SERVER_MBEAN_NAME);
                if ( mbeanServer.isRegistered(objName) ) {
                    // unregister service provider
                    Object[] params = {"type"};
                    String[] sigs = {"java.lang.String"};
                    mbeanServer.invoke(objName,"unregisterServiceProvider",params,sigs);
                }
            } catch(Exception e) {
                e.printStackTrace();
            }
        }
    }
    
    public ObjectName preRegister(MBeanServer server, ObjectName objName)
            throws Exception {
        mbeanServer = server;
        return objName;
    }    
    
    public void postRegister(Boolean registrationDone) {
    }
    
    
    public void preDeregister() throws Exception {
    }    
    
    public void postDeregister() {
    }        
    
}
