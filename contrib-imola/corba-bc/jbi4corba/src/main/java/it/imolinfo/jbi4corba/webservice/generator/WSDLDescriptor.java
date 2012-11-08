 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;
import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.jbi.Messages;

import java.util.Properties;

/**
 * Descriptor to incapsulate all fields required to generate a WSDL file
 * starting from an IDL (CORBA) file.
 *
 * @author  <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
public class WSDLDescriptor {

    private final String corbaServiceName;

    private final String localizationType;
    
    private final String namespace;

    private final String endpointName;

    private Properties orbProperties;
    
    /** The logger. */
    private static final Logger LOG = 
    	LoggerFactory.getLogger(WSDLDescriptor.class);
    private static final Messages MESSAGES = 
    	Messages.getMessages(WSDLDescriptor.class);

    /**
     * 
     * @param corbaServiceName  The corba service name
     * @param localizationType  The Localization Type
     * @param namespace         The name space
     * @param endpointName      The endpoint name
     */
    public WSDLDescriptor(String corbaServiceName, String localizationType,
                          String namespace, String endpointName) {
        if (corbaServiceName == null) {
        	String msg=MESSAGES.getString("CRB000567_Corba_Service_Name_null");
            LOG.error(msg);
            throw new NullPointerException(msg);
        }
        if (localizationType == null) {
        	String msg=MESSAGES.getString("CRB000568_Localization_Type_null");
            LOG.error(msg);
            throw new NullPointerException(msg);
        }
        if (namespace == null) {
        	String msg=MESSAGES.getString("CRB000569_Namespace_null");
            LOG.error(msg);
            throw new NullPointerException(msg);
        }
        if (endpointName == null) {
        	String msg=MESSAGES.getString("CRB000570_Endpoint_name_null");
            LOG.error(msg);
            throw new NullPointerException(msg);
        }
        
        this.corbaServiceName = corbaServiceName;
        this.localizationType = localizationType;
        this.namespace        = namespace;
        this.endpointName     = endpointName;
    }

    /**
     * 
     * @return  The return
     */
    public String getCorbaServiceName() {
        return corbaServiceName;
    }
    
        /**
     * 
     * @return  The return
     */
    public String getLocalizationType() {
        return localizationType;
    }

    /**
     * 
     * @return  The return
     */
    public String getEndpointName() {
        return endpointName;
    }

    /**
     * 
     * @return  The return
     */
    public String getNamespace() {
        return namespace;
    }

    /**
     * 
     * @return  The return
     */
    public Properties getOrbProperties() {
        if (orbProperties == null) {
            orbProperties = new Properties();
        }
        return orbProperties;
    }

    /**
     * 
     * @param orbProperties  The orb properties
     */
    public void setOrbProperties(Properties orbProperties) {
        this.orbProperties = orbProperties;
    }
}
