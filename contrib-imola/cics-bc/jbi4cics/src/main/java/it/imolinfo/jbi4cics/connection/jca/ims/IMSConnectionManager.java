/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/

package it.imolinfo.jbi4cics.connection.jca.ims;

import it.imolinfo.jbi4cics.connection.jca.JCACommareaBasedConnectionManager;
import it.imolinfo.jbi4cics.exception.ConnectionException;
import it.imolinfo.jbi4cics.service.ServiceContext;

import javax.resource.cci.ConnectionSpec;
import javax.resource.cci.InteractionSpec;
import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;



/**
 * 
 * @author raffaele, marcopiraccini
 */
public class IMSConnectionManager extends JCACommareaBasedConnectionManager {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(IMSConnectionManager.class);
    
    public IMSConnectionManager() {
        super();
    }

    /* (non-Javadoc)
     * @see it.imolinfo.jbi4cics.connection.jca.JCACommareaBasedConnectionManager#createInteractionSpec(it.imolinfo.jbi4cics.service.ServiceContext)
     */
    protected InteractionSpec createInteractionSpec(ServiceContext serviceContext) throws ConnectionException {

        InteractionSpec imsInteractionSpec = null;

        try {
            imsInteractionSpec = (InteractionSpec)
            this.getConnectorClassLoader().loadClass("com.ibm.connector2.ims.ico.IMSInteractionSpec").newInstance();
        } catch (InstantiationException e) {
            LOG.error("CIC000500_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);
            throw new ConnectionException("CIC000500_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);                 
        } catch (IllegalAccessException e) {
            LOG.error("CIC000500_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);
            throw new ConnectionException("CIC000500_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);                 
        } catch (ClassNotFoundException e) {
            LOG.error("CIC000500_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);
            throw new ConnectionException("CIC000500_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);                 
        }   

        /*
         * property to set:
         * username
         * password
         * see http://publib.boulder.ibm.com/infocenter/cicstg60/index.jsp 
         */
        if (imsInteractionSpec != null) {
            //TODO sets the propety correctly
            /*
             * The settable properties are: 
             * AsyncOutputAvailable
             * CommitMode
             * ConvEnded
             * ExecutionTimeout
             * IMSRequestType
             * InteractionVerb
             * LtermName
             * MapName
             * SocketTimeot
             */    

        } else {      
            LOG.error("CIC000501_InteractionSpec_not_retrieved");
            throw new ConnectionException("CIC000501_InteractionSpec_not_retrieved");               
        }
        return imsInteractionSpec;

    }

    /* (non-Javadoc)
     * @see it.imolinfo.jbi4cics.connection.jca.JCACommareaBasedConnectionManager#createConnectionSpec(it.imolinfo.jbi4cics.service.ServiceContext)
     */
    protected ConnectionSpec createConnectionSpec(ServiceContext serviceContext) throws ConnectionException {
        ConnectionSpec imsConnectionSpec = null;

        try {
            imsConnectionSpec = (ConnectionSpec)
            this.getConnectorClassLoader().loadClass("com.ibm.connector2.ims.ico.IMSConnectionSpec").newInstance();
        } catch (InstantiationException e) {
            LOG.error("CIC000500_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);
            throw new ConnectionException("CIC000500_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);                 
        } catch (IllegalAccessException e) {
            LOG.error("CIC000500_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);
            throw new ConnectionException("CIC000500_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);                 
        } catch (ClassNotFoundException e) {
            LOG.error("CIC000500_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);
            throw new ConnectionException("CIC000500_Error_istantiating_connection_spec", new Object[] {e.getMessage()}, e);                 
        }   

        if (imsConnectionSpec != null) {
            // TODO sets the property correclty
            /*
             * the properties that can be set from the interface are:
             * user name
             * password
             * group name (a part of ims security)
             * clientid (don't know what exactly is) 
             * see http://publib.boulder.ibm.com/infocenter/dzichelp/v2r2/index.jsp
             */

            // imsConnectionSpec.setClientID(??);
            // imsConnectionSpec.setGroupName(??);

        } else {      
            LOG.error("CIC000502_ConnectionSpec_not_retrieved");
            throw new ConnectionException("CIC000502_ConnectionSpec_not_retrieved");               
        }

        return imsConnectionSpec;
    }

}
