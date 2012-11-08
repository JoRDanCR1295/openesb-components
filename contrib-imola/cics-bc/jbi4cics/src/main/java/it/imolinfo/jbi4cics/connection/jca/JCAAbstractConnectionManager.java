/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
/**
 * 
 */
package it.imolinfo.jbi4cics.connection.jca;

import it.imolinfo.jbi4cics.connection.ConnectionManager;
import it.imolinfo.jbi4cics.exception.ConnectionException;
import it.imolinfo.jbi4cics.locator.ServiceLocation;
import it.imolinfo.jbi4cics.service.ServiceContext;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.resource.ResourceException;
import javax.resource.cci.Connection;
import javax.resource.cci.ConnectionFactory;
import javax.resource.cci.ConnectionSpec;
import javax.resource.cci.Interaction;
import javax.resource.cci.InteractionSpec;
import javax.resource.cci.Record;
import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;


/**
 * @author raffaele, marcopiraccini
 */
public abstract class JCAAbstractConnectionManager implements ConnectionManager {
	   

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(JCAAbstractConnectionManager.class);
	
    ConnectionFactory connectionFactory = null;	
  
    public JCAAbstractConnectionManager() {
        super();
	}

	/* (non-Javadoc)
	 * 	@see it.imolinfo.jbi4cics.connection.ConnectionManager#handleCall(it.imolinfo.jbi4cics.service.ServiceContext)
	 */
    public void handleCall(ServiceContext serviceContext) throws ConnectionException{
        Connection connection=null;
        Interaction interaction=null;
        if (connectionFactory == null) {
            lookupConnectionFactory(serviceContext);
        }
        try{
            ConnectionSpec connectionSpec = createConnectionSpec(serviceContext);
            connection = createConnection(serviceContext,connectionSpec);
            interaction=createInteraction(connection);
            InteractionSpec interactionSpec=createInteractionSpec(serviceContext);
            // creo record di input e output
            Record inputRecord=createInputRecord(serviceContext);
            Record outputRecord=createOutputRecord(serviceContext);      
            // TODO outputRecord.setCommareaLength(interactionSpec.getReplyLength());
            try{
                boolean result=interaction.execute(interactionSpec,inputRecord,outputRecord);
                if (!result){
                    LOG.error("CIC000302_Wrong_execution_of_the_request");
                    throw new ConnectionException("CIC000302_Wrong_execution_of_the_request");
                }
            }
            catch(ResourceException e){
                //TODO probably, we should manage the exception...
                LOG.error("CIC000303_Error_executing_the_request", new Object[] {e.getMessage()}, e);
                throw new ConnectionException("CIC000303_Error_executing_the_request", new Object[] {e.getMessage()}, e);
            }
            Object outputMessage=createOutputMessage(serviceContext,outputRecord);
            serviceContext.setOutputMessage(outputMessage);
        }
        finally {
            releaseResources(connection,interaction);
        }
    }



    protected void releaseResources(Connection connection, Interaction interaction) throws ConnectionException {
        try {
            if (interaction!=null){
                interaction.close();
            }
            if (connection!=null){
                connection.close();
            }
        }
        catch (ResourceException e){
            LOG.error("CIC000304_Error_releasing_resources", new Object[] {e.getMessage()}, e);
            throw new ConnectionException("CIC000304_Error_releasing_resources", new Object[] {e.getMessage()}, e);
        }
    }

    protected Interaction createInteraction(Connection connection) throws ConnectionException {
        try {
            Interaction interaction = connection.createInteraction();
            return interaction;
        }
        catch (ResourceException e) {
            LOG.error("CIC000305_Error_getting_interaction_from_JCA", new Object[] {e.getMessage()}, e);
            throw new ConnectionException("CIC000305_Error_getting_interaction_from_JCA", new Object[] {e.getMessage()}, e);
        }
    }  

    protected Connection createConnection(ServiceContext serviceContext, ConnectionSpec connectionSpec) throws ConnectionException{
        // per prima cosa ottengo la service location
        if (connectionFactory == null) {
            lookupConnectionFactory(serviceContext);    
        }
        try {
            // TODO vedere se servono le connectio properties
            Connection connection = connectionFactory.getConnection(connectionSpec);
            return connection;
        }
        catch (ResourceException e) {
            LOG.error("CIC000306_Error_getting_connection_from_JCA", new Object[] {e.getMessage()}, e);
            throw new ConnectionException("CIC000306_Error_getting_connection_from_JCA", new Object[] {e.getMessage()}, e);
        }
    }

    /**
     * loads the <code>ConnectionFactory</code> using the configured JNDI name.  
     * @param serviceContext    The service context
     * @throws ConnectionException    The connection exception
     */
    private void lookupConnectionFactory(ServiceContext serviceContext) throws ConnectionException {
        ServiceLocation serviceLocation=serviceContext.getServiceLocation();

        if (connectionFactory == null) {
            // Gets the JNDI name
            String jndiConnectionName = serviceLocation.getLocationName();

            try {
                InitialContext initialContext = new javax.naming.InitialContext();
                connectionFactory = (ConnectionFactory)initialContext.lookup(jndiConnectionName);
            }
            catch (NamingException e) {
                LOG.error("CIC000307_Error_retrieving_JCA_connection_factory", new Object[] {e.getMessage()}, e);
                throw new ConnectionException("CIC000307_Error_retrieving_JCA_connection_factory", new Object[] {e.getMessage()}, e);
            }
            if (connectionFactory == null) {
                LOG.error("CIC000308_Lookup_failed", new Object[] {jndiConnectionName});
                throw new ConnectionException("CIC000308_Lookup_failed", new Object[] {jndiConnectionName});
            }
        }
    }  


    /**
     * Return the <code>ConnectionFactory</code> class loader.
     * @return    The class loader
     * @throws ConnectionException    The connection exception
     */
    public ClassLoader getConnectorClassLoader() throws ConnectionException {

        if (connectionFactory == null) {
            throw new ConnectionException("CIC000309_Null_connection_factory");
        }
        return connectionFactory.getClass().getClassLoader();	  	 
    }

    /**
     * @param serviceContext    The service context
     * @param outputRecord    The output record
     * @return    The created output message
     * @throws ConnectionException    The connection exception
     */
    protected abstract Object createOutputMessage(ServiceContext serviceContext, Record outputRecord) throws ConnectionException;

    /**
     * @param serviceContext    The service context
     * @throws ConnectionException    The connection exception
     * @return InteractionSpec Iteraction spec
     */  
    protected abstract InteractionSpec createInteractionSpec(ServiceContext serviceContext) throws ConnectionException;

    /**
     * @param serviceContext    The service context
     * @throws ConnectionException    The connection exception
     * @return Record    The output record created
     */  
    protected abstract Record createOutputRecord(ServiceContext serviceContext) throws ConnectionException;

    /**
     * @param serviceContext    The service context
     * @throws ConnectionException    The connection exception
     * @return Record    The input record created
     */  
    protected abstract Record createInputRecord(ServiceContext serviceContext) throws ConnectionException;

    /**
     * @param serviceContext    The service context
     * @throws ConnectionException    The connection exception
     * @return ConnectionSpec     The connection spec
     */  
    protected abstract ConnectionSpec createConnectionSpec(ServiceContext serviceContext) throws ConnectionException; 

}
