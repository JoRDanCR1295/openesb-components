/****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.wsdl;

import java.io.Serializable;
import java.util.Properties;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;

/**
 * The Class Jbi4CorbaAddress, according with JWSDL specs (see JSR 110).
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4CorbaAddress implements ExtensibilityElement, Serializable {

    /** serialVersionUID. */
    private static final long serialVersionUID = 6856599663215008314L;

    /** The element type. */
    private QName elementType;

    /** The required. */
    private Boolean required;

    /** The name of the corba service. */
    private String name;

    /** How to locate the corba service. */
    private String localizationType;

    /** The ORB properties */
    private Properties orbProperties;

    /**
     * The default constructor.
     */
    public Jbi4CorbaAddress() {
        // NOP
    }

    // getter and setter

    /**
     * @return The return
     */
    public QName getElementType() {
        return elementType;
    }

    /**
     * @param elementType
     *            The element type
     */
    public void setElementType(QName elementType) {
        this.elementType = elementType;
    }

    /**
     * @return The return
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            The name
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return The return
     */
    public Boolean getRequired() {
        return required;
    }

    /**
     * @param required
     *            The required
     */
    public void setRequired(Boolean required) {
        this.required = required;
    }

    /**
     * @return The return
     */
    public String getLocalizationType() {
        return localizationType;
    }

    /**
     * 
     * @param localizationType
     *            The localization type
     */
    public void setLocalizationType(String localizationType) {
        this.localizationType = localizationType;
    }

    /**
     * @return The return
     */
    public String toString() {
        return ReflectionToStringBuilder.toString(this);
    }

    /**
     * @param obj
     *            The object
     * @return The return
     */
    public boolean equals(Object obj) {
        return EqualsBuilder.reflectionEquals(this, obj);
    }

    /**
     * @return The return
     */
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(17, 37, this);
    }

    /**
     * @return The return
     */
    public Properties getOrbProperties() {
        return orbProperties;
    }

    /**
     * @param orbProperties
     *            The orb properties
     */
    public void setOrbProperties(Properties orbProperties) {
        this.orbProperties = orbProperties;
    }

}
