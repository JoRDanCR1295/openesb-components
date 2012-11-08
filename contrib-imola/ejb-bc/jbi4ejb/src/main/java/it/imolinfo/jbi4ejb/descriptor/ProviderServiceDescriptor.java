/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.descriptor;

import it.imolinfo.jbi4ejb.runtime.ejbproxy.StatelessEJBProxy;

import java.util.Properties;

import javax.xml.namespace.QName;


/**
 * The service descriptor class.
 * 
 * @author marco
 */
public class ProviderServiceDescriptor { 
        
    // From types
    /** The serial version UID. */
    private Properties serialVersionUID = new Properties();
    
    // From the binding
    /** The orb properties. */
    private Properties orbProperties;
        
    /** The jndi properties. */
    private Properties jndiProperties;
    
    // From the address
    /** The name. */
    private String name;
    
    /** The localization type. */
    private String localizationType;    

    // The component root path
    /** The component root path. */
    private String componentRootPath;
    
    /** The service name. */
    private QName serviceName;
    
    /** The port type name. */
    private QName portTypeName = null;
    
    /** The ejb proxy. */
    private StatelessEJBProxy ejbProxy = null;
    
    /**
     * Instantiates a new provider service descriptor.
     */
    public ProviderServiceDescriptor() {}    

    /**
     * Gets the component root path.
     * 
     * @return the component root path
     */
    public String getComponentRootPath() {
        return componentRootPath;
    }

    /**
     * Sets the component root path.
     * 
     * @param componentRootPath
     *            the new component root path
     */
    public void setComponentRootPath(String componentRootPath) {
        this.componentRootPath = componentRootPath;
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
     * Sets the service name.
     * 
     * @param serviceName
     *            the new service name
     */
    public void setServiceName(QName serviceName) {
        this.serviceName = serviceName;
    }

    /**
     * Gets the serial version UID.
     * 
     * @return the serial version UID
     */
    public Properties getSerialVersionUID() {
        return serialVersionUID;
    }

    /**
     * Sets the serial version UID.
     * 
     * @param serialVersionUID
     *            the new serial version UID
     */
    public void setSerialVersionUID(Properties serialVersionUID) {
        this.serialVersionUID = serialVersionUID;
    }

    /**
     * Gets the orb properties.
     * 
     * @return the orb properties
     */
    public Properties getOrbProperties() {
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
     * Sets the name.
     * 
     * @param name
     *            the new name
     */
    public void setName(String name) {
        this.name = name;
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
     * Sets the localization type.
     * 
     * @param localizationType
     *            the new localization type
     */
    public void setLocalizationType(String localizationType) {
        this.localizationType = localizationType;
    }

    /**
     * Gets the port type name.
     * 
     * @return the port type name
     */
    public QName getPortTypeName() {
        return portTypeName;
    }

    /**
     * Sets the port type name.
     * 
     * @param portTypeName
     *            the new port type name
     */
    public void setPortTypeName(QName portTypeName) {
        this.portTypeName = portTypeName;
    }

    /**
     * Gets the ejb proxy.
     * 
     * @return the ejb proxy
     */
    public StatelessEJBProxy getEjbProxy() {
        return ejbProxy;
    }

    /**
     * Sets the ejb proxy.
     * 
     * @param ejbProxy
     *            the new ejb proxy
     */
    public void setEjbProxy(StatelessEJBProxy ejbProxy) {
        this.ejbProxy = ejbProxy;
    }       
           
}
