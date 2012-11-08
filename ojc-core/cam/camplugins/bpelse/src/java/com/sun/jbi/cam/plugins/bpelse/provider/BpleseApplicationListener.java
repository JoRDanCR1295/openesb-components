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
 * @(#)BpleseApplicationListener.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.provider;

import com.sun.jbi.cam.common.AnnotatedStandardMBean;
import java.io.File;
import java.util.StringTokenizer;
import java.util.logging.Logger;
import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.MBeanServerConnection;
import javax.management.MBeanServerFactory;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.management.ReflectionException;
import javax.servlet.ServletContext;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;

/**
 * Actual mbean implementation code
 *
 * @author ylee
 */
public class BpleseApplicationListener implements ServletContextListener {
    
    private Logger logger = Logger.getLogger(BpleseApplicationListener.class.getName());
    
    private static String PLUGIN_CONTEXT_STRING = "/bpelse";
    private static String MBEAN_NAME = "com.sun.cam:type=plugins,name=bpelse";
    private static String MBEAN_CLASS_NAME = "com.sun.jbi.cam.plugins.bpelse.BpelseServiceProvider";
    
    
    public void contextInitialized(ServletContextEvent event) {
        ///////////////////////////////////////////
        // Ensure no one else can use this Listener
        // except this web app
        ServletContext ctx = event.getServletContext();
        String contextPath = ctx.getContextPath();
        if(contextPath.endsWith(PLUGIN_CONTEXT_STRING) == false) {
            System.out.println("Context "+contextPath+" does not match "+PLUGIN_CONTEXT_STRING);
            return;
        }
        ///////////////////////////////////////////
        logger.info("\n\n\n\nInitialiazing context "+contextPath);
        registerApplication(event);
    }
    
    public void contextDestroyed(ServletContextEvent event) {
        /////////////////////////////////////////// //////////////
        // Ensure no one else can use this Listener
        // except this web app
        ServletContext ctx = event.getServletContext();
        String contextPath = ctx.getContextPath();
        if(contextPath.endsWith(PLUGIN_CONTEXT_STRING) == false) {
            System.out.println("Context "+contextPath+" does not match "+PLUGIN_CONTEXT_STRING);
            return;
        }
        /////////////////////////////////////////// //////////////
        logger.info("\n\n\n\nDestroying context "+contextPath);
        unregisterApplication(event);
    }
    
    
    protected void registerApplication(ServletContextEvent event) {

        ///////////////////////////////////////////
        // Ensure no one else can use this Listener
        // except this web app
        ServletContext ctx = event.getServletContext();
        String contextPath = ctx.getContextPath();
        if(contextPath.endsWith(PLUGIN_CONTEXT_STRING) == false) {
            System.out.println("Context "+contextPath+" does not match "+PLUGIN_CONTEXT_STRING);
            return;
        }
        ///////////////////////////////////////////
        
        MBeanServer mbeanServer = null;
        ObjectName objName = null;
        BpelseServiceProvider mbean = null;
        Object annotatedMbean = null;
        // 1. create mbean:
        mbeanServer = getLocalServerConnector();
        try {
            objName = new ObjectName(MBEAN_NAME);
        } catch (MalformedObjectNameException ex) {
            ex.printStackTrace();
        } catch (NullPointerException ex) {
            ex.printStackTrace();
        }
        try {
            
            mbean = new BpelseServiceProvider();
        } catch (NotCompliantMBeanException ex) {
            ex.printStackTrace();
        }
        try {
            annotatedMbean = new AnnotatedStandardMBean(mbean,
                    BpelseServiceProviderMBean.class);
        } catch(NotCompliantMBeanException ex) {
            ex.printStackTrace();
        }
        
        if(false == mbeanServer.isRegistered(objName)) {
            try {
                mbeanServer.registerMBean(annotatedMbean,objName);
                logger.info("MBean: "+MBEAN_NAME+" created and registered.");
            } catch (NotCompliantMBeanException ex) {
                ex.printStackTrace();
            } catch (InstanceAlreadyExistsException ex) {
                ex.printStackTrace();
            } catch (MBeanRegistrationException ex) {
                ex.printStackTrace();
            }
        }
        
        // 2. register service provider
        Object[] params = {};
        String[] sigs = {};
        try {
            mbeanServer.invoke(objName,"registerServiceProvider",params,sigs);
        } catch (InstanceNotFoundException ex) {
            ex.printStackTrace();
        } catch (ReflectionException ex) {
            ex.printStackTrace();
        } catch (MBeanException ex) {
            ex.printStackTrace();
        }
        
        
    }
    
    
    protected void unregisterApplication(ServletContextEvent event) {

        ///////////////////////////////////////////
        // Ensure no one else can use this Listener
        // except this web app
        
        ServletContext ctx = event.getServletContext();
        String contextPath = ctx.getContextPath();
        if(contextPath.endsWith(PLUGIN_CONTEXT_STRING) == false) {
            System.out.println("Context "+contextPath+" does not match "+PLUGIN_CONTEXT_STRING);
            return;
        }
        ///////////////////////////////////////////
        
        MBeanServer mbeanServer = null;
        ObjectName objName = null;
        
        mbeanServer = getLocalServerConnector();
        try {
            objName = new ObjectName(MBEAN_NAME);
        } catch (MalformedObjectNameException ex) {
            ex.printStackTrace();
        } catch (NullPointerException ex) {
            ex.printStackTrace();
        }

        // 1. deregister service provider
        Object[] params = {};
        String[] sigs = {};
        try {
            mbeanServer.invoke(objName,"unregisterServiceProvider",params,sigs);
        } catch (InstanceNotFoundException ex) {
            ex.printStackTrace();
        } catch (MBeanException ex) {
            ex.printStackTrace();
        } catch (ReflectionException ex) {
            ex.printStackTrace();
        }
            
        // 2. remove mbean
        if (true == mbeanServer.isRegistered(objName)) {
            try {
                mbeanServer.unregisterMBean(objName);
                logger.info("MBean: "+MBEAN_NAME+" unregistered.");
            } catch (MBeanRegistrationException ex) {
                ex.printStackTrace();
            } catch (InstanceNotFoundException ex) {
                ex.printStackTrace();
            }
        }
    }
    
    
    private MBeanServer getLocalServerConnector() {
        return (MBeanServer)MBeanServerFactory.findMBeanServer(null).get(0);
    }
}
