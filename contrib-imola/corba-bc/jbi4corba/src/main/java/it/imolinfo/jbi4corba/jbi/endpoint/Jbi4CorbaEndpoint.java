 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.endpoint;

import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.jbi.component.Jbi4CorbaSUManager;
import it.imolinfo.jbi4corba.jbi.processor.ExchangeProcessor;
import javax.jbi.messaging.MessageExchange.Role;

import java.io.File;
import java.io.Serializable;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;

import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.eManager.provider.StatusProviderHelper;

/**
 * The Class Jbi4CorbaEndpoint.
 */
public abstract class Jbi4CorbaEndpoint implements Serializable {
        

	/** serialVersionUID. */
	private static final long serialVersionUID = -365758605104761399L;

	/** The Constant SHUTDOWN. */
    public static final int SHUTDOWN = 0;
    
    /** The Constant STOPPED. */
    public static final int STOPPED = 1;
    
    /** The Constant RUNNING. */
    public static final int RUNNING = 2;    
               
    /** The service name. */
    private QName serviceName = null;
    
    /** The endpoint name. */
    private String endpointName = null;
    
    /** The definition. */
    private Definition definition;

    /** The state. */
    private int state;
    
    /** The endpoint status. */
    private EndpointStatus endpointStatus = null;
    
    // JBI

    /** The service endpoint. */
    private ServiceEndpoint serviceEndpoint = null;
    
    /** The service description. */
    private Document serviceDescription = null;
    
    // The service unit name
    /** The su name. */
    private String suName = null;
    
    /** The su manager. */
    private Jbi4CorbaSUManager suManager = null;
    
    /** The exchange processor. */
    private ExchangeProcessor exchangeProcessor = null;
    
    /** The endpoint WSDL. */
    private File endpointWSDL = null;
    
    
    /**
     * Empty constructor.
     */
    public Jbi4CorbaEndpoint() {        
    } 
    
        
    /**
     * Instantiates a new jbi4 Corba endpoint.
     * 
     * @param serviceName the service name
     * @param endpointName the endpoint name
     */
    public Jbi4CorbaEndpoint(QName serviceName, String endpointName) {
        this.serviceName = serviceName;
        this.endpointName = endpointName;
    }    
    
    /**
     * Utility method to create the unique names with explicit arguments.
     * 
     * @return the unoque name
     */
    public String getUniqueName() {
        String serviceNamespaceURI = getServiceName().getNamespaceURI();
        String serviceName = getServiceName().getLocalPart();
        String endpointName = getEndpointName();
        return getUniqueName(serviceNamespaceURI, serviceName, endpointName, getRole().equals(Role.PROVIDER)? true:false);
    }    

    public static String getUniqueName(String aServiceNamespaceURI,
                                       String aServiceName,
                                       String aEndpointName, 
                                       boolean isInbound) {
        String aEndpointType =
            isInbound ? StatusProviderHelper.PROVISIONING_ID : StatusProviderHelper.CONSUMING_ID ;
        return aServiceNamespaceURI + "," + aServiceName + "," + aEndpointName + "," + aEndpointType;
    }

    /**
     * Gets the service name.
     * 
     * @return the service name
     */
    public QName getServiceName() {
        return serviceName;
    }

    /**
     * Gets the endpoint name.
     * 
     * @return the endpoint name
     */
    public String getEndpointName() {
        return endpointName;
    }

    /**
     * Gets the definition.
     * 
     * @return the definition
     */
    public Definition getDefinition() {
        return definition;
    }

    /**
     * Sets the definition.
     * 
     * @param definition the new definition
     */
    public void setDefinition(Definition definition) {
        this.definition = definition;
    }

    /**
     * Gets the state.
     * 
     * @return the state
     */
    public int getState() {
        return state;
    }

    /**
     * Sets the state.
     * 
     * @param state the new state
     */
    public void setState(int state) {
        this.state = state;
    }

    /**
     * Gets the endpoint status.
     * 
     * @return the endpoint status
     */
    public EndpointStatus getEndpointStatus() {
        return endpointStatus;
    }

    /**
     * Sets the endpoint status.
     * 
     * @param endpointStatus the new endpoint status
     */
    public void setEndpointStatus(EndpointStatus endpointStatus) {
        this.endpointStatus = endpointStatus;
    }

    /**
     * Gets the service endpoint.
     * 
     * @return the service endpoint
     */
    public ServiceEndpoint getServiceEndpoint() {
        return serviceEndpoint;
    }

    /**
     * Sets the service endpoint.
     * 
     * @param serviceEndpoint the new service endpoint
     */
    public void setServiceEndpoint(ServiceEndpoint serviceEndpoint) {
        this.serviceEndpoint = serviceEndpoint;
    }

    /**
     * Gets the service description.
     * 
     * @return the service description
     */
    public Document getServiceDescription() {
        return serviceDescription;
    }

    /**
     * Sets the service description.
     * 
     * @param serviceDescription the new service description
     */
    public void setServiceDescription(Document serviceDescription) {
        this.serviceDescription = serviceDescription;
    }
               
    /**
     * Gets the su name.
     * 
     * @return the su name
     */
    public String getSuName() {
        return suName;
    }

    /**
     * Sets the su name.
     * 
     * @param suName the new su name
     */
    public void setSuName(String suName) {
        this.suName = suName;
    }
           
    /**
     * Gets the su manager.
     * 
     * @return the su manager
     */
    public Jbi4CorbaSUManager getSuManager() {
        return suManager;
    }


    /**
     * Sets the su manager.
     * 
     * @param suManager the new su manager
     */
    public void setSuManager(Jbi4CorbaSUManager suManager) {
        this.suManager = suManager;
    }       

    /**
     * Gets the exchange processor.
     * 
     * @return the exchange processor
     */
    public ExchangeProcessor getExchangeProcessor() {
        return exchangeProcessor;
    }

    /**
     * Sets the exchange processor.
     * 
     * @param exchangeProcessor the new exchange processor
     */
    public void setExchangeProcessor(ExchangeProcessor exchangeProcessor) {
        this.exchangeProcessor = exchangeProcessor;
    }
        
    /**
     * Gets the endpoint WSDL.
     * 
     * @return the endpoint WSDL
     */
    public File getEndpointWSDL() {
        return endpointWSDL;
    }

    /**
     * Sets the endpoint WSDL.
     * 
     * @param endpointWSDL the new endpoint WSDL
     */
    public void setEndpointWSDL(File endpointWSDL) {
        this.endpointWSDL = endpointWSDL;
    }

    /* (non-Javadoc)
     * 
     */
    
    
    
    /**
     * the endpoints are equals if the servicename, the endpointname and the 
     * role are the same.
     * 
     * @param obj the object to compare
     * 
     * @return true if the objects are equals
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
        if (obj instanceof Jbi4CorbaEndpoint) {
            Jbi4CorbaEndpoint c = (Jbi4CorbaEndpoint) obj;
            if ((this.serviceName.equals(c.serviceName)) &&                 
            (this.endpointName.equals(c.endpointName)) && 
            (this.getRole().equals(c.getRole()))) {
                return true;
            }
          }
          return false;
    }
    
    /**
     * hashcode implementation.
     * 
     * @return the object hashcode
     * 
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return this.serviceName.hashCode() ^ this.endpointName.hashCode() ^ this.getRole().hashCode();
    }
        
    public String toString() {
        return this.serviceName+ ":" + endpointName +"/" + (getRole().equals(Role.PROVIDER)?"PROVIDER":"CONSUMER");
    }      
    
    /**
     * Register service.
     * 
     * @throws Jbi4CorbaException if something go wrong
     */
    public abstract void registerService() throws Jbi4CorbaException;
    
    /**
     * Unregister service.
     * 
     * @throws Jbi4CorbaException if something go wrong
     */
    public abstract void unregisterService() throws Jbi4CorbaException;
    
    /**
     * Validate.
     * 
     * @throws Jbi4CorbaException if something go wrong
     */
    public abstract void validate() throws Jbi4CorbaException;    
    
    /**
     * Activate the endpoint.
     * 
     * @throws Jbi4CorbaException
     */
    public abstract void activate() throws Jbi4CorbaException;
    
    /**
     * Deactivate the endpoint.
     * 
     * @throws Jbi4CorbaException
     */
    public abstract void deactivate() throws Jbi4CorbaException;    

    /**
     * Gets the role in MessageExchange.
     * @return
     */
    public abstract Role getRole();     
    

}
