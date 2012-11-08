/*
 * GlassFishHttpServerConnector.java
 * 
 * Created on Jul 5, 2007, 3:13:17 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.em.connectors;

import java.io.Serializable;

/**
 *
 * @author ylee
 */
public class GlassFishHttpServerConnector  extends HttpServerConnector implements Serializable {
    
    public static final String GF_HTTP_PROTOCOL = "s1ashttp"; //$NON-NLS-1$    

    public GlassFishHttpServerConnector() {
    }
    
    public GlassFishHttpServerConnector(String type, String serverName, String hostName, String port, String instancePort, String userName, String userPassword) {
        super(type, serverName, hostName, port, instancePort, userName, userPassword);
    }      

    protected String getProtocol() {
        return GF_HTTP_PROTOCOL;
    }    
    
}
