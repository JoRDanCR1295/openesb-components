/*
 * ChainBuilder ESB
 *          Visual Enterprise Integration
 * 
 * Copyright (C) 2006 Bostech Corporation
 * 
 * This program is free software; you can redistribute it and/or modify it 
 * under the terms of the GNU General Public License as published by the 
 * Free Software Foundation; either version 2 of the License, or (at your option) 
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License 
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License along with 
 * this program; if not, write to the Free Software Foundation, Inc., 
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *
 * $Id: BaseCommonAttribute.java,v 1.1.1.1 2007/04/09 17:49:31 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.component.util.wsdl;

import javax.jbi.messaging.MessageExchange.Role;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import java.io.Serializable;
import java.net.URI;

public class BaseCommonAttribute implements ExtensibilityElement, Serializable {

    /**
     * Generated serial version UID
     */
    private static final long serialVersionUID = -3118867357618475968L;
    
    protected Boolean required;
    protected QName elementType;
    
    protected Role role;
    protected URI defaultMep;
    protected QName defaultOperation;
    
    /**
     * @return Returns the elementType.
     */
    public QName getElementType() {
        return elementType;
    }
    /**
     * @param elementType The elementType to set.
     */
    public void setElementType(QName elementType) {
        this.elementType = elementType;
    }
    /**
     * @return Returns the required.
     */
    public Boolean getRequired() {
        return required;
    }
    /**
     * @param required The required to set.
     */
    public void setRequired(Boolean required) {
        this.required = required;
    }
    public Role getRole() {
        return role;
    }
    public void setRole(Role role) {
        this.role = role;
    }
    /**
     * @return Returns the defaultMep.
     */
    public URI getDefaultMep() {
        return defaultMep;
    }
    /**
     * @param defaultMep The defaultMep to set.
     */
    public void setDefaultMep(URI defaultMep) {
        this.defaultMep = defaultMep;
    }
    
    /**
     * @return Returns the defaultOperation.
     */
    public QName getDefaultOperation() {
        return defaultOperation;
    }
    /**
     * @param defaultOperation The defaultOperation to set.
     */
    public void setDefaultOperation(QName defaultOperation) {
        this.defaultOperation = defaultOperation;
    }
    
    public String toString() {
        return "BaseCommonAttribute[" + 
                    "required=" + required + ", " +
                    "elementType=" + elementType + ", " +
                    "role=" + role + ", " +
                    "defaultMep=" + defaultMep + "," +
                    "defaultOperation=" + defaultOperation + "]";
    }
    
}
