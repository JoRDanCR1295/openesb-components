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
 * @(#)Types.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.xsd.XMLSchema;

import java.util.Collection;


/**
 * Models the WSDL &lt;types&gt; element.
 * This is a container of zero or more XML Schemas.
 * 
 * @author Sun Microsystems
 * @version 
 */
public interface Types extends WSDLExtensibleElement {
        
    /** Tag for this element */
    public static final String TAG = "types";
    
    /**
     * Gets index of XML Schema element within list.
     * @param   xmlSch  XML Schema element to index
     * @return  Index of XML Schema element within list.
     */
    int indexOfSchema(XMLNode xmlSch);

    /**
     * Gets the collection of all XML Schemas.
     * @return a read-only collection of XMLSchemas.
     */
    Collection getSchemas();
    
    
    /**
     * Gets the schema at the specified location.
     * @param index the index into the location
     * @return the schema
     * @throws IndexOutOfBoundsException if index is out of bounds
     */
    XMLSchema getSchema(int index) throws IndexOutOfBoundsException;
    
    
    /**
     * Adds a schema.
     * @param schema to add
     */
    void addSchema(XMLSchema schema);
    
    
    /**
     * Removes the schema at the specified index.
     * @param index the index to the location
     * @return true if removed
     * @throws IndexOutOfBoundsException if index is out of bounds
     */
    boolean removeSchema(int index) throws IndexOutOfBoundsException;
    
    
    /**
     * Removes the schema.
     * @param schema to remove
     * @return true if removed
     */
    boolean removeSchema(XMLSchema schema);
    
    
    /**
     * Gets the number of schemas.
     * @return the number of schemas
     */
    int getSchemaCount();
}
