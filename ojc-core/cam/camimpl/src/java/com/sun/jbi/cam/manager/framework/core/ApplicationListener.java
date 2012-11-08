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

package com.sun.jbi.cam.manager.framework.core;

import com.sun.jbi.cam.manager.framework.core.mbeans.ServerMBean;
import com.sun.jbi.cam.manager.framework.core.mbeans.Server;
import java.util.StringTokenizer;
import java.util.logging.Logger;
import javax.management.MBeanServer;
import javax.management.MBeanServerConnection;
import javax.management.MBeanServerFactory;
import javax.management.ObjectName;
import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;



/**
 *
 * @author ylee
 * @author graj
 */
public class ApplicationListener implements ServletContextListener {

    private static String CAM_CONTEXT_STRING = "/cam";
    private static String CAM_SERVER_MBEAN_NAME = "com.sun.cam:type=server,name=ServerMBean";
    private static String MBEAN_CLASS_NAME = "com.sun.jbi.cam.manager.framework.core.mbeans.Server";

    private Logger logger = Logger.getLogger(ApplicationListener.class.getName());
    private ComponentServiceProviderResolver resolver = null;
    
    
    public void contextInitialized(ServletContextEvent event) {
        ServletContext ctx = event.getServletContext();
        String contextPath = ctx.getContextPath();
        if(contextPath.endsWith(CAM_CONTEXT_STRING) == false) {
            System.out.println("Context "+contextPath+" does not match "+CAM_CONTEXT_STRING);
            return;
        }
        logger.info("\n\n\n\nInitialiazing context "+contextPath);
        registerApplication(event);
    }

    public void contextDestroyed(ServletContextEvent event) {
        ServletContext ctx = event.getServletContext();
        String contextPath = ctx.getContextPath();
        if(contextPath.endsWith(CAM_CONTEXT_STRING) == false) {
            System.out.println("Context "+contextPath+" does not match "+CAM_CONTEXT_STRING);
            return;
        }
        logger.info("\n\n\n\nDestroying context "+contextPath);
    }

    protected void registerApplication(ServletContextEvent event) {
        ServletContext ctx = event.getServletContext();
        String contextPath = ctx.getContextPath();
        if(contextPath.endsWith(CAM_CONTEXT_STRING) == false) {
            System.out.println("Context "+contextPath+" does not match "+CAM_CONTEXT_STRING);
            return;
        }
        resolver = ComponentServiceProviderResolver.getInstance();
        
        // create ServerMBean
         try {
             
            MBeanServer mbeanServer = getLocalServerConnector(); 
            ObjectName objName = new ObjectName(CAM_SERVER_MBEAN_NAME);
            ServerMBean mbean = new Server();
            // Ensure that the MBean is not registered before attempting
            // to register the MBean
            if(false == mbeanServer.isRegistered(objName)) {
                mbeanServer.registerMBean(mbean,objName);
                logger.info("MBean: "+CAM_SERVER_MBEAN_NAME+" created and registered.");
            }
            
         } catch(Exception e) {
             e.printStackTrace();
         }        
        
    }
    
    protected void unregisterApplication(ServletContextEvent event) {
        ServletContext ctx = event.getServletContext();
        String contextPath = ctx.getContextPath();
        if(contextPath.endsWith(CAM_CONTEXT_STRING) == false) {
            System.out.println("Context "+contextPath+" does not match "+CAM_CONTEXT_STRING);
            return;
        }
        resolver = null;
        
        // unregister ServerMBean
	try {
            MBeanServerConnection connection = getLocalServerConnector();
            ObjectName objName = new ObjectName(CAM_SERVER_MBEAN_NAME);
            if ( connection.isRegistered(objName)) {
                connection.unregisterMBean(objName);
                logger.info("MBean: "+CAM_SERVER_MBEAN_NAME+" unregistered.");
            }
                    
        } catch(Exception e) {
            e.printStackTrace();
        }        
    }

    
    private MBeanServer getLocalServerConnector() {
          return (MBeanServer)MBeanServerFactory.findMBeanServer(null).get(0);
    }    

}
