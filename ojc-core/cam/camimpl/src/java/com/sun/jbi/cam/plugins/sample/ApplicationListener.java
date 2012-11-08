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
 * @(#)ApplicationListener.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.sample;

import java.util.StringTokenizer;
import java.util.logging.Logger;
import javax.management.MBeanServer;
import javax.management.MBeanServerConnection;
import javax.management.MBeanServerFactory;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;



/**
 *
 * @author ylee
 */
public class ApplicationListener implements ServletContextListener {

    private Logger logger = Logger.getLogger(ApplicationListener.class.getName());
    
    private static String MBEAN_NAME = "com.sun.cam:type=plugins,name=sample";
    private static String MBEAN_CLASS_NAME = "com.sun.cam.plugins.sample.Sample";
	
    
    public void contextInitialized(ServletContextEvent event) {
        logger.info("\n\n\n\nInitialiazing context :::");
        registerApplication(event);
    }

    public void contextDestroyed(ServletContextEvent event) {
        ServletContext ctx = event.getServletContext();
        logger.info("\n\n\n\nDestroying context :::");
        unregisterApplication(event);
    }


    protected void registerApplication(ServletContextEvent event) {
        ServletContext ctx = event.getServletContext();
        
         try {
            // 1. create mbean: 
            MBeanServer mbeanServer = getLocalServerConnector(); 
            ObjectName objName = new ObjectName(MBEAN_NAME);
            
            SampleMBean mbean = new Sample();
            mbeanServer.registerMBean(mbean,objName);
            logger.info("MBean: "+MBEAN_NAME+" created and registered.");
            
            // 2. register service provider
            Object[] params = {};
            String[] sigs = {};
            mbeanServer.invoke(objName,"registerServiceProvider",params,sigs);
            
         } catch(Exception e) {
             //
         }

    }

	
    protected void unregisterApplication(ServletContextEvent event) {
        
	try {
            // 1. remove mbean
            MBeanServer mbeanServer = getLocalServerConnector();
            ObjectName objName = new ObjectName(MBEAN_NAME);
            if ( mbeanServer.isRegistered(objName)) {
                mbeanServer.unregisterMBean(objName);
                logger.info("MBean: "+MBEAN_NAME+" unregistered.");
            }
            
            // 2. deregister service provider
            Object[] params = {};
            String[] sigs = {};
            mbeanServer.invoke(objName,"unregisterServiceProvider",params,sigs);
                    
        } catch(Exception e) {
            //
        }
    }
    
    
    private MBeanServer getLocalServerConnector() {
          return (MBeanServer)MBeanServerFactory.findMBeanServer(null).get(0);
    }


}
