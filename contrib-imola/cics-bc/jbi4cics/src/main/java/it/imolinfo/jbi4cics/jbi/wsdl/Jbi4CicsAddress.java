/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/

package it.imolinfo.jbi4cics.jbi.wsdl;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;

/**
 * The bean represanting the WSDL address extension.
 * TODO: Add serialVersionUID
 * 
 * @author amedeocannone, marcopiraccini
 */
public class Jbi4CicsAddress implements ExtensibilityElement, Serializable {	
    /** The element type. */
    private QName elementType;

    /** The required. */
    private Boolean required;
    
    private String username;
    
    private String password;
    
    private String connectionType;
    
    private String JNDIConnectionName;

    private String programName;
    
    private String transactionName;
    
    private Boolean tpn;                /**     * void constructor.     */      public Jbi4CicsAddress(){    	  super();      }
    
    public QName getElementType() {
        return elementType;
    }

    public void setElementType(QName elementType) {
        this.elementType = elementType;
    }

    public Boolean getRequired() {
        return required;
    }

    public void setRequired(Boolean required) {
        this.required = required;
    }

    public String getConnectionType() {
        return connectionType;
    }

    public void setConnectionType(String connectionType) {
        this.connectionType = connectionType;
    }

    public String getJNDIConnectionName() {
        return JNDIConnectionName;
    }

    public void setJNDIConnectionName(String connectionName) {
        JNDIConnectionName = connectionName;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getProgramName() {
        return programName;
    }

    public void setProgramName(String programName) {
        this.programName = programName;
    }

    public Boolean getTpn() {
        return tpn;
    }

    public void setTpn(Boolean tpn) {
        this.tpn = tpn;
    }

    public String getTransactionName() {
        return transactionName;
    }

    public void setTransactionName(String transactionName) {
        this.transactionName = transactionName;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }
    
    public String toString() {
        return ReflectionToStringBuilder.toString(this);
    }
    
    public boolean equals(Object obj) {
        return EqualsBuilder.reflectionEquals(this, obj);
    }
    
    public int hashCode() {
        return HashCodeBuilder.reflectionHashCode(17, 37, this);
    }    
        

}
