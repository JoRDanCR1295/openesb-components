/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.etl.engine.utils;

import java.util.HashMap;
import java.util.Properties;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.Map;
import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

/**
 *
 * @author admin
 */
public class ConnectionPoolConfigLookup {

    private static MBeanServerConnection mbeanServer = null; // MBeans
   
    public static void initMBeanServer(Properties jmxProps) {
        try {
            //Get the JMX details from runtimeMBean
            String jmxHost = (String)jmxProps.get("JmxHost");
            String jmxPort = (String)jmxProps.get("JmxPort");
            String jmxUser = (String)jmxProps.get("JmxUser");
            String jmxPassword = (String)jmxProps.get("JmxPassword");
            
            HashMap<String, String[]> env = new HashMap<String, String[]>();
            String[] credentials = new String[]{jmxUser, jmxPassword};
            env.put(JMXConnector.CREDENTIALS, credentials);
            JMXServiceURL url = new JMXServiceURL("service:jmx:rmi:///jndi/rmi://"+
                    jmxHost + ":" + jmxPort + "/jmxrmi");
            JMXConnector jmxc = JMXConnectorFactory.connect(url, env);
            mbeanServer = jmxc.getMBeanServerConnection();
        } catch (MalformedURLException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static MBeanServerConnection getConnection() {
        return mbeanServer;
    }

    private static String getPoolName(String jndiResName) {
        String poolName = null;
        if (jndiResName != null && !"".equalsIgnoreCase(jndiResName)) {
            try {
                String objectNameStr = "amx:j2eeType=X-JDBCResourceConfig,name=" + jndiResName;
                ObjectName mbeanObj = new ObjectName(objectNameStr);
                poolName = (String) mbeanServer.getAttribute(mbeanObj, "PoolName");
            } catch (Throwable e) {
                e.printStackTrace();
            }
        }
        return poolName;
    }

    public static Map getConnectionPoolProperties(String jndiResName) throws Exception {
        if(jndiResName == null || "".equalsIgnoreCase(jndiResName)) return new HashMap();
        String objectNameStr = "amx:j2eeType=X-JDBCConnectionPoolConfig,name=" + getPoolName(jndiResName);
        ObjectName mbeanObj = new ObjectName(objectNameStr);
        Map props = (Map) mbeanServer.getAttribute(mbeanObj, "Properties");
        return props;
    }
}
