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


import javax.management.MBeanServerConnection;

import java.io.IOException;
import java.util.Properties;
import javax.management.j2ee.Management;

/**
 *
 * @author  Yoke Lee
 */


public interface AbstractConnector extends MBeanServerConnection {
	
    public abstract AbstractConnector getConnector() throws Exception;
    
    public abstract AbstractConnector getConnector(Properties env) throws Exception;
    
    public Management getManagement();
    
    public MBeanServerConnection getMBeanServerConnection();

    public String getProtocol();
    
    public String getHostName();
    
    public String getPort();
    
    public String getUserName();
    
    public String getUserPassword();
    
    public String getJndiName();
    
    public String getDefaultDomain() throws IOException, java.rmi.RemoteException;
    
    public int getHostType();
 
}
