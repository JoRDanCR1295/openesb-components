/*
 *
 * Copyright 2007 Sun Microsystems, Inc. All rights reserved.
 */

package com.sun.em.connectors;

import java.io.Serializable;
import javax.management.MBeanServerConnection;

/**
 *
 * @author ylee
 */
public abstract class ServerConnector implements Serializable {

    protected String hostName;
    protected String adminPort;                  // admin port
    protected String userName;
    protected String userPassword;
    protected String type;
    protected String serverName;
    protected String instancePort = "8080";     // default
    protected String jmxPort = "8686";          // default
    protected String clusterName;
    
    protected transient MBeanServerConnection connection;
    
    public ServerConnector() {
    }
    
    public ServerConnector(String type, String serverName, String hostName, String port, String instancePort, String userName, String userPassword) {
        this.type = type;
        this.serverName = serverName;
        this.hostName = hostName;
        this.adminPort = port;
        this.instancePort = instancePort;
        this.userName = userName;
        this.userPassword = userPassword;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }
    
    public String getHostName() {
        return hostName;
    }

    public String getAdminPort() {
        return adminPort;
    }
    
    public int getAdminPortValue() {
         int portValue = -1;
         try {
            portValue = new Integer(this.getAdminPort()).intValue();
         } catch(Exception e) {
             e.printStackTrace();
         }
         return portValue;
    }

    public String getUserName() {
        return userName;
    }

    public String getUserPassword() {
        return userPassword;
    }

    public void setHostName(String hostName) {
        this.hostName = hostName;
    }

    public void setAdminPort(String port) {
        this.adminPort = port;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public void setUserPassword(String userPassword) {
        this.userPassword = userPassword;
    }

    public abstract MBeanServerConnection getConnection();
    

    public void setConnection(MBeanServerConnection connection) {
        this.connection = connection;
    }

    public String getServerName() {
        return serverName;
    }

    public String getInstancePort() {
        return instancePort;
    }

    public void setServerName(String serverName) {
        this.serverName = serverName;
    }

    public void setInstancePort(String instancePort) {
        this.instancePort = instancePort;
    }

    public String getJmxPort() {
        return jmxPort;
    }

    public void setJmxPort(String jmxPort) {
        this.jmxPort = jmxPort;
    }

    public String getClusterName() {
        return clusterName;
    }

    public void setClusterName(String clusterName) {
        this.clusterName = clusterName;
    }
    
    
    public String getKey() {
        return getServerName();
    }

}
