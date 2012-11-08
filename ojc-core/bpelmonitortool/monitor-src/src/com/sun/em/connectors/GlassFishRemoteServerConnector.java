/*
 *
 * Copyright 2007 Sun Microsystems, Inc. All rights reserved.
 */

package com.sun.em.connectors;

import com.sun.em.common.GenericConstants;
import java.io.Serializable;
import javax.management.MBeanServerConnection;

/**
 *
 * @author ylee
 */
public class GlassFishRemoteServerConnector extends RemoteServerConnector implements Serializable {

    public GlassFishRemoteServerConnector() {
    }

    public GlassFishRemoteServerConnector(String type, String serverName, String hostName, String port, String instancePort, String userName, String userPassword) {
        super(type, serverName, hostName, port, instancePort, userName, userPassword);
    }    

    @Override
    protected String getHttpConnectionType() {
        return "s1ashttp";
    }

    @Override
    protected String getHttpsConnectionType() {
        return "s1ashttps";
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        String hostName = "localhost", userName="admin", password="adminadmin";
        int portNumber = 4848;
        MBeanServerConnection connection = null;
        GlassFishRemoteServerConnector remoteConnector = new GlassFishRemoteServerConnector(GenericConstants.SJSAS_SERVER_TYPE,
                 "server1",hostName, portNumber+"", "8080", userName, password);
        connection = remoteConnector.getConnection();
        if(connection != null) {
            System.out.println("*** connection: "+connection.toString());
        } else {
            System.out.println("Connection to MBeanServer could not be retrieved.");
        }
    }     

}
