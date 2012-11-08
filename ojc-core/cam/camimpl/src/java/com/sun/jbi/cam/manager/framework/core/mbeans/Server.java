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
 * @(#)Server.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.core.mbeans;

import com.sun.jbi.cam.manager.framework.core.ComponentServiceProviderResolver;
import java.util.*;
import java.util.logging.Logger;
import javax.management.MBeanRegistration;
import javax.management.MBeanServer;
import javax.management.ObjectName;

/**
 *
 * @author ylee
 */
public class Server implements ServerMBean, MBeanRegistration {
    
    private MBeanServer mbeanServer;
    
    private Logger logger = Logger.getLogger(Server.class.getName());
    
    private static String CAM_SERVER_MBEAN_NAME = "com.sun.cam:type=server,name=ServerMBean";
    private static String CAM_DOMAIN_PREFIX = "com.sun.cam:";
    private static String CAM_PLUGINS_FILTER = "type=plugins,*";
    
    private ComponentServiceProviderResolver resolver;
    private Thread workerThread = null;    
    
    
    /** Creates a new instance of ServerMBeanImpl */
    public Server() {
        resolver = ComponentServiceProviderResolver.getInstance();
    }


    public void registerServiceProvider(String type, String url) {
        // add to static service provider map
         logger.info(">>>>> adding: type="+type+" url="+url+" to service provider map");
         resolver.addProvider(type,url);
    }
    
    public void unregisterServiceProvider(String type) {
        // remove to static service provider map
        logger.info(">>>>> removing: "+type+" from service provider map");
        resolver.removeProvider(type);
    }
    
    public String getProviderUrl(String name,String type,
                                    String componentName,
                                    String componentType) {
        return  resolver.getProviderUrl(
                            name,
                            type,
                            componentName,
                            componentType);           
        
    }
    
    public ObjectName preRegister(MBeanServer server, ObjectName objName)
            throws Exception {
        mbeanServer = server;
        return objName;
    }    
    
    public void postRegister(Boolean registrationDone) {
        startWorkerThread();
    }
    
    
    public void preDeregister() throws Exception {
    }    
    
    public void postDeregister() {
    }            
    
    
    private void getPluginRegistrations() {
        // get all deployed plugins registrations
        try {
            ObjectName filter = new ObjectName(CAM_DOMAIN_PREFIX + CAM_PLUGINS_FILTER);
            Set objNames = mbeanServer.queryNames(filter, null);
            if ( objNames!=null && objNames.isEmpty()==false ) {
                for (Iterator iter = objNames.iterator(); iter.hasNext(); ) {
                    ObjectName objName = (ObjectName)iter.next();
                    try {
                        logger.info("objName:"+objName.getCanonicalName());
                        mbeanServer.invoke(objName,"registerServiceProvider",null,null);
                    } catch(Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        } catch(Exception e) {
            e.printStackTrace();
        }
    }
    
    
    private void startWorkerThread() {
        workerThread = new Thread() {
            public void run() {
                logger.info("starting worker thread to retrieve plugin registrations...");
                // wait for 1 seconds
                try {
                    Thread.sleep(1000);
                } catch(InterruptedException e) {
                }
                getPluginRegistrations();
                logger.info("worker thread ends.");
            }
        };
        
        workerThread.start();
    }
    
    
}
