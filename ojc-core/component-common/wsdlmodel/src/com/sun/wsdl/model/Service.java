/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)Service.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import com.sun.wsdl.model.common.model.XMLNode;

import java.util.Collection;

/**
 * Describes the &lt;service&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Service
    extends NamedWSDLExtensibleElement {
        
    /** Tag for this element */
    public static final String TAG = "service";
    
    /** Ordinal position of name attribute. */
    public static final int NAME = 0;
    
    /**
     * Getter for property name.
     * @return Value of property name.
     */
    String getName();
    
    /**
     * Setter for property name.
     * @param name   New value of property name.
     */
    void setName(String name);
    
    
    /**
     * Adds a new port.
     * @param port the port to add
     */
    void addPort(ServicePort port);
    
    
    /**
     * Adds a new port at the end of collection.
     * @param port the port to add
     */
    void addPortAtTail(ServicePort port);
    	
    /**
     * Removes the given port.
     * @param port to be removed
     */
    void removePort(ServicePort port);
    
    /**
     * Gets the list of all ports.
     * @return a read-only collection of ServicePorts.
     */
    Collection getPorts();
}
