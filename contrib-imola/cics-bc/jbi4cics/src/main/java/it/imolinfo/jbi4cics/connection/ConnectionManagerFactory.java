/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
/**
 * 
 */
package it.imolinfo.jbi4cics.connection;

import it.imolinfo.jbi4cics.connection.jca.cics.CICSConnectionManager;
import it.imolinfo.jbi4cics.connection.jca.ims.IMSConnectionManager;
import it.imolinfo.jbi4cics.connection.jdbc.JdbcConnectionManager;
import it.imolinfo.jbi4cics.locator.ServiceLocation;
import it.imolinfo.jbi4cics.service.ServiceContext;
import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;

/**
 * @author raffaele
 *
 */
public class ConnectionManagerFactory {
	/**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(ConnectionManagerFactory.class);
	
    private static CICSConnectionManager cicsConnectionManager=new CICSConnectionManager();
    private static IMSConnectionManager imsConnectionManager=new IMSConnectionManager();
    private static JdbcConnectionManager jdbcConnectionManager=new JdbcConnectionManager();
    private static EchoConnectionManager echoConnectionManager=new EchoConnectionManager();
	
    /**
	 * void constructor.
	 */
	  public ConnectionManagerFactory(){
		  super();
	  }

    
    public static ConnectionManager createConnectionManager(ServiceContext serviceContext){
        
        LOG.debug("Using service location: {0}", serviceContext.getServiceLocation());       
        LOG.debug("Using connection type: {0}", serviceContext.getServiceLocation().getConnectionType());
        switch (serviceContext.getServiceLocation().getConnectionType()){
        case ServiceLocation.CICS : return cicsConnectionManager;
        case ServiceLocation.IMS  : return imsConnectionManager;
        case ServiceLocation.JDBC  : return jdbcConnectionManager;
        default : return echoConnectionManager;
        }
    }
}
