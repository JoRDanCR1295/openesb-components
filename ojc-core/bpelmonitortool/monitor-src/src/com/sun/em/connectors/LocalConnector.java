/*
 *
 * Copyright 2007 Sun Microsystems, Inc. All rights reserved.
 */

package com.sun.em.connectors;

import java.io.Serializable;
import javax.management.MBeanServerConnection;
import javax.management.MBeanServerFactory;

/**
 *
 * @author ylee
 */
public class LocalConnector extends ServerConnector implements Serializable {
    
    public LocalConnector() {
    }
    
    public LocalConnector(String type, String serverName, String hostName, String port, String instancePort, String userName, String userPassword) {
        super(type, serverName, hostName, port, instancePort, userName, userPassword);
    }
    
    public MBeanServerConnection getConnection() {
        if ( connection != null ) {
            // connect to local server
             connection = (MBeanServerConnection)MBeanServerFactory.findMBeanServer(null).get(0);
        }
        return connection;
    }
    
}
