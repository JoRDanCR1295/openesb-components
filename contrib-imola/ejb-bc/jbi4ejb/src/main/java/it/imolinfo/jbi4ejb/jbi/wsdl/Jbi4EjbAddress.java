/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.jbi.wsdl;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;

/**
 * The Class Jbi4EjbAddress, according with JWSDL specs (see JSR 110).
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4EjbAddress implements ExtensibilityElement, Serializable {

	/** The Constant serialVersionUID. */
    private static final long serialVersionUID = -1553032069918902750L;

    /** The element type. */
	private QName elementType;

	/** The required. */
	private Boolean required;

	/** The name. */
	private String name;
    
    /** The localization type. */
    private String localizationType;

    
    /**
     * Instantiates a new jbi4 ejb address.
     */
    public Jbi4EjbAddress() {}

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

    /* (non-Javadoc)
     * @see javax.wsdl.extensions.ExtensibilityElement#getRequired()
     */
    
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
