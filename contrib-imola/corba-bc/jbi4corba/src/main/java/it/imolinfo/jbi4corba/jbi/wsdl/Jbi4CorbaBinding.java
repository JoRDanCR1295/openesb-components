/****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.wsdl;

import java.io.Serializable;

import java.util.ArrayList;
import java.util.List;
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
public class Jbi4CorbaBinding implements ExtensibilityElement, Serializable {

    /** serialVersionUID. */
    private static final long serialVersionUID = 4322482627377876884L;

    /** The element type. */
    private QName elementType;

    /** The required. */
    private Boolean required;

    public List<Jbi4CorbaIDLEntry> getJbi4CorbaDLEntryList() {
        return Jbi4CorbaDLEntryList;
    }

    public void setJbi4CorbaDLEntryList(List<Jbi4CorbaIDLEntry> Jbi4CorbaDLEntryList) {
        this.Jbi4CorbaDLEntryList = Jbi4CorbaDLEntryList;
    }

    /** The Corba idl. */
    //private String idl;

    private List<Jbi4CorbaIDLEntry> Jbi4CorbaDLEntryList=new ArrayList<Jbi4CorbaIDLEntry>();

    /**
     * Default constructor.
     */
    public Jbi4CorbaBinding() {
    }

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
}
