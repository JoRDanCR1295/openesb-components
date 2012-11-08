/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.webservice.generator;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.exception.ClassGenerationException;
import it.imolinfo.jbi4ejb.jbi.Messages;

import java.util.Properties;

/**
 * Descriptor to incapsulate all fields required to extend a WSDL file
 * starting from a ejb RemoteInterface.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class WSDLDescriptor {
	
	/** Logger. */
    static final Logger LOG = LoggerFactory.getLogger(WSDLDescriptor.class);
    private static final Messages MESSAGES
    = Messages.getMessages(WSDLDescriptor.class);
    
    /** The Constant CORBANAME_LOCALIZATION_TYPE. */
    public final static String CORBANAME_LOCALIZATION_TYPE = "corbaname";
    
    /** The Constant JNDI_LOCALIZATION_TYPE. */
    public final static String JNDI_LOCALIZATION_TYPE = "jndi";

    /** The corba service name. */
    private final String name;

    /** The corba service name. */
    private final String localizationType;    

    /** The orb properties. */
    private Properties orbProperties;
    
    /** The jndi properties. */
    private Properties jndiProperties;       

    /**
     * Instantiates a new WSDL descriptor.
     * 
     * @param name
     *            The name
     * @param localizationType
     *            The localization type
     */
    public WSDLDescriptor(String name, String localizationType) {
        if (name == null) {
        	String msg=MESSAGES.getString("EJB001016_Ejb_Service_Name_null");
            LOG.error(msg);
            throw new NullPointerException(msg);
        }        
        if (localizationType == null) {
        	String msg=MESSAGES.getString("EJB001017_Localization_type_name_null");
            LOG.error(msg);
            throw new NullPointerException(msg);
        }
  
        this.name = name;
        this.localizationType = localizationType;
    }

    /**
     * Gets the corba service name.
     * 
     * @return the corba service name
     */
    public String getCorbaServiceName() {
        return name;
    }   

    /**
     * Gets the orb properties.typesSerialVersionUIDs.
     * 
     * @return the orb properties
     */
    public Properties getOrbProperties() {
        if (orbProperties == null) {
            orbProperties = new Properties();
        }
        return orbProperties;
    }

    /**
     * Sets the orb properties.
     * 
     * @param orbProperties
     *            the new orb properties
     */
    public void setOrbProperties(Properties orbProperties) {
        this.orbProperties = orbProperties;
    }
    
    /**
     * Gets the jndi properties.
     * 
     * @return the jndi properties
     */
    public Properties getJndiProperties() {
        return jndiProperties;
    }

    /**
     * Sets the jndi properties.
     * 
     * @param jndiProperties
     *            the new jndi properties
     */
    public void setJndiProperties(Properties jndiProperties) {
        this.jndiProperties = jndiProperties;
    }

    /**
     * Gets the name.
     * 
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * Gets the localization type.
     * 
     * @return the localization type
     */
    public String getLocalizationType() {
        return localizationType;
    }
    
    /**
     * True if the localilzation type is woth corba name.
     * 
     * @return true, if the localilzation type is woth corba name
     */
    public boolean isCorbaName() {
        if (CORBANAME_LOCALIZATION_TYPE.equalsIgnoreCase(this.getLocalizationType())) {
            return true;
        } 
        return false;
    }
    
}
