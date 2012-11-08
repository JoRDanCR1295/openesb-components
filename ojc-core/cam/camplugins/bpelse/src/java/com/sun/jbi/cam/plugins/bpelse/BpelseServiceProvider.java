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
 * @(#)BpelseServiceProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.logging.Logger;
import javax.faces.context.FacesContext;
import javax.management.MBeanRegistration;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;
import javax.management.StandardMBean;

/**
 *
 * @author ylee
 */
public class BpelseServiceProvider extends StandardMBean implements BpelseServiceProviderMBean, MBeanRegistration {
    
    private MBeanServer mbeanServer;
    
    private Logger logger = Logger.getLogger(BpelseServiceProvider.class.getName());
    
    private static String CAM_SERVER_MBEAN_NAME = "com.sun.cam:type=server,name=ServerMBean";
    private static String PROVIDER_MAPPINGS_FILENAME = "providerMappings.properties";
    /** The ResourceBundle basename for this web app. */
    public static final String BASENAME = "com.sun.jbi.cam.plugins.bpelse.resources.providerMappings";
    
    private Map<String, String> mappings = new HashMap<String, String>();
    
    /**
     * Creates a new instance of AspectsServiceProvider
     */
    public BpelseServiceProvider() throws NotCompliantMBeanException {
        super(BpelseServiceProviderMBean.class);
    }
    
    public void registerServiceProvider() {
        // Read properties file.
        ResourceBundle bundle = ResourceBundle.getBundle(BASENAME, getLocale());
        if (bundle == null) {
            throw new NullPointerException("registerServiceProvider:Could not obtain resource bundle");
        }
        
        Enumeration enumeration = null;
        String key = null, value = null, type = null;
        ObjectName objName = null;
        try {
            objName = new ObjectName(CAM_SERVER_MBEAN_NAME);
        } catch (MalformedObjectNameException ex) {
            ex.printStackTrace();
        } catch (NullPointerException ex) {
            ex.printStackTrace();
        }
        if(objName == null) {
            System.out.println("registerServiceProvider: "+CAM_SERVER_MBEAN_NAME+" objectName cannot be created.");
            return;
        }
        mbeanServer = getLocalServerConnector();
        if ( mbeanServer!=null ) {
            if ( mbeanServer.isRegistered(objName) == true ) {
                // loop through properties and
                // register service providers
                enumeration = bundle.getKeys();
                while (enumeration.hasMoreElements() == true) {
                    key = (String) enumeration.nextElement();
                    value = bundle.getString(key);
                    if((key != null) && (key.length() > 0)
                    && (value != null) && (value.length() > 0)) {
                        type = key.replace("_", ":");
                        System.out.println("Register {type,url} = {"+type+", "+value+"}");
                        mappings.put(type, value);
                        Object[] params = {type, value};
                        String[] signatures = {"java.lang.String","java.lang.String"};
                        try {
                            mbeanServer.invoke(objName,"registerServiceProvider",params,signatures);
                        } catch(Exception e) {
                            e.printStackTrace();
                        }
                    } else {
                        System.out.println("Register {type,url} = {"+type+", "+value+"}");
                    }
                }
            } else {
                System.out.println("registerServiceProvider: "+CAM_SERVER_MBEAN_NAME+" is not registered.");
            }
        } else {
            System.out.println("registerServiceProvider: MBeanServer is null");
        }
    }
    
    public void unregisterServiceProvider() {
        ObjectName objName = null;
        try {
            objName = new ObjectName(CAM_SERVER_MBEAN_NAME);
        } catch (MalformedObjectNameException ex) {
            ex.printStackTrace();
        } catch (NullPointerException ex) {
            ex.printStackTrace();
        }
        if(objName == null) {
            return;
        }
        mbeanServer = getLocalServerConnector();
        if ( mbeanServer!=null ) {
            if ( mbeanServer.isRegistered(objName) == true ) {
                // unregister service provider
                Set<String> set = this.mappings.keySet();
                if(set != null) {
                    for(Iterator<String> iterator = set.iterator();
                           iterator.hasNext() == true;) {
                        String type = iterator.next();
                        if(type != null) {
                            System.out.println("Unregister type = {"+type+"}");
                            Object[] params = {type};
                            String[] sigs = {"java.lang.String"};
                            try {
                                mbeanServer.invoke(objName,"unregisterServiceProvider",params,sigs);
                            } catch(Exception e) {
                                e.printStackTrace();
                            }
                        }
                    }
                }
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
    
    /** Get locale from the FacesContext object. */
    protected static Locale getLocale() {
        FacesContext context = FacesContext.getCurrentInstance();
        if (context == null) {
            return Locale.getDefault();
        }
        
        // context.getViewRoot() may not have been initialized at this point.
        Locale locale = null;
        if (context.getViewRoot() != null) {
            locale = context.getViewRoot().getLocale();
        }
        
        return (locale != null) ? locale : Locale.getDefault();
    }

    private MBeanServer getLocalServerConnector() {
          return (MBeanServer)MBeanServerFactory.findMBeanServer(null).get(0);
    }    
    
}
