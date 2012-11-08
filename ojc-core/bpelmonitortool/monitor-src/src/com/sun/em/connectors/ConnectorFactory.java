/* *************************************************************************
 *
 *          Copyright (c) 2002, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.em.connectors;


import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;
import javax.naming.Context;

import java.util.logging.Logger;



/**
 * Connector Factory.
 * @author ylee
 */
public class ConnectorFactory {
    
    private static Logger mLogger = Logger.getLogger(ConnectorFactory.class.getName());

    
    /**
     * Get an instance of the ServerConnector (based on the JSR160 compliant JMXConnector)
     * @param hostname
     * @param port
     * @return ServerConnector
     */
    public static AbstractConnector getServerConnector(String protocol, String hostname,String port,
            String username,String password,String contextFactoryClassName,String jndiName,
            int hostType) 
        throws Exception {

        Properties env = new Properties();
        if ( contextFactoryClassName!=null ) {
        	env.put(Context.INITIAL_CONTEXT_FACTORY,contextFactoryClassName);
        }
        if ( !"0".equals(port) ) {
        	env.put(Context.PROVIDER_URL, protocol+ "://" + hostname + ":" + port);
        }
        if (username != null) {
            env.put(Context.SECURITY_PRINCIPAL, username);
        }
        if (password != null) {
            env.put(Context.SECURITY_CREDENTIALS, password);
        }
        JmxConnector jmxConnector = new JmxConnector(
           protocol,hostname,port,username,password,jndiName,hostType);
        return jmxConnector.getConnector(env);
    }

    
    /**
     * JSR160 compliant JMXConnector
     * @author ylee
     * return an instance of JMXConnector
     */
    public static JMXConnector getJMXConnector(String protocol, String hostname, String port, 
            String username, String password, String contextFactoryClassName,String jndiName) {

        Map env = new HashMap();
        env.put(Context.INITIAL_CONTEXT_FACTORY,contextFactoryClassName);
        env.put(Context.PROVIDER_URL, protocol+ "://" + hostname + ":" + port);
        if (username != null) {
            env.put(Context.SECURITY_PRINCIPAL, username);
        }
        if (password != null) {
            env.put(Context.SECURITY_CREDENTIALS, password);
        }
        JMXConnector connector = null;
        try {
            JMXServiceURL serviceURL;
            if ( jndiName!=null ) {
                serviceURL = new JMXServiceURL(protocol, hostname, Integer.parseInt(port),jndiName);
            } else {
                serviceURL = new JMXServiceURL(protocol, hostname, Integer.parseInt(port));
            }
            connector = JMXConnectorFactory.connect(serviceURL,env);
        } catch(Exception e) {
            e.printStackTrace();   
        }
        return connector;
    }
    
}

