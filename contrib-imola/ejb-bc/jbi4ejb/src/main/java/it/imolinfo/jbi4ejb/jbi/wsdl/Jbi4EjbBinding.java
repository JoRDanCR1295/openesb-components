/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.jbi.wsdl;

import java.io.Serializable;
import java.util.Properties;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;

/**
 * The Class Jbi4CorbaBinding, according with JWSDL specs (see JSR 110).
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4EjbBinding implements ExtensibilityElement, Serializable {

	/** The Constant serialVersionUID. */
    private static final long serialVersionUID = -6147135464051010433L;

    /** The element type. */
	private QName elementType;

	/** The required. */
	private Boolean required;

	/** The orb properties. */
	private Properties orbProperties;
	
	/** The jndi properties. */
	private Properties jndiProperties;
	
	/**
     * Instantiates a new jbi4 ejb binding.
     */
	public Jbi4EjbBinding() {}
	
	
    /**
     * Gets the ElementType.
     * @see javax.wsdl.extensions.ExtensibilityElement#getElementType()
     * @return the extensibility element
     */
    public QName getElementType() {
        return elementType;
    }
    
    /**
     * Sets the element type.
     * 
     * @param elementType the element type
     * 
     * @see javax.wsdl.extensions.ExtensibilityElement#setElementType(javax.xml.namespace.QName)
     */
    public void setElementType(QName elementType) {
        this.elementType = elementType;
    }

    /**
     * Required property getter.
     * @see javax.wsdl.extensions.ExtensibilityElement#getRequired()
     * @return true if required
     */
    public Boolean getRequired() {
        return required;
    }
    
    /**
     * Required property setter.
     * 
     * @param required the required property
     * 
     * @see javax.wsdl.extensions.ExtensibilityElement#setRequired(java.lang.Boolean)
     */
    public void setRequired(Boolean required) {
        this.required = required;
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
     * toString redefinition.
     * @see java.lang.Object#toString()
     * @return the object strig rapresentation
     */
    public String toString() {
        return ReflectionToStringBuilder.toString(this);
    }
    
    /**
     * equals redefinition.
     * 
     * @param obj the parameter to test
     * 
     * @return true if the two object are equals
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
        return EqualsBuilder.reflectionEquals(this, obj);
    }
    
    
    /**
     * hashCode redefinition.
     * @see java.lang.Object#hashCode()
     * @return the hashcode for the object instance
     */
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(17, 37, this);
    }

}
