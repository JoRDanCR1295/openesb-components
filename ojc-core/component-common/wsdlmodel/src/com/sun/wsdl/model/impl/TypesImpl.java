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
 * @(#)TypesImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;


 

import com.sun.wsdl.model.Types;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.visitor.WSDLVisitor;
import com.sun.wsdl.model.xsd.XMLSchema;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import java.util.logging.Logger;




/**
 * Implements the Types interface to represent the WSDL &lt;types&gt; element.
 * 
 * @author Sun Microsystems
 * @version 
 */
public class TypesImpl 
    extends WSDLExtensibleElementImpl
    implements Types {
        
    /** The logger. */
    private static Logger mLogger = Logger.getLogger(TypesImpl.class.getName());
        
    /** List of schemas. */
    List mSchemas;
    
    
    /**
     * Creates a new instance of TypesImpl.
     * @param   d   Owner document.
     */
    public TypesImpl(XMLDocument d) {
        super(d);
        initTypes();
    }
    
    
    /**
     * Initializes this class.
     */
    private void initTypes() {
        setLocalName(Types.TAG);
        //setNamespace(WSDLDocument.WSDL_NAMESPACE);
        mSchemas = new ArrayList();
    }
    
    
    /**
     * Adds a child of the element.
     * @param   c   Child.
     */
    public void addChild(XMLNode c) {
        super.addChild(c);
    }
    
    
    /**
     * Removes a child of the element
     * @param   c   Child.
     */
    public void removeChild(XMLNode c) {
        super.removeChild(c);
    }
    
    /**
     * @see Types#indexOfSchema
     */
    public int indexOfSchema(XMLNode xmlSch) {
        return mSchemas.indexOf(xmlSch);
    }

    /**
     * Gets the collection of all XML Schemas.
     * @return a read-only collection of XMLSchemas.
     */
    public Collection getSchemas() {
        return Collections.unmodifiableCollection(mSchemas);
    }
    
    
    /**
     * Gets the schema at the specified location.
     * @param index the index into the location
     * @return the schema
     * @throws IndexOutOfBoundsException if index is out of bounds
     */
    public XMLSchema getSchema(int index) throws IndexOutOfBoundsException  {
        return (XMLSchema) mSchemas.get(index);
    }
    
    
    /**
     * Adds a schema.
     * @param schema to add
     */
    public void addSchema(XMLSchema schema) {
        mSchemas.add(schema);
    }
    
    
    /**
     * Removes the schema at the specified index.
     * @param index the index to the location
     * @return true if removed
     * @throws IndexOutOfBoundsException if index is out of bounds
     */
    public boolean removeSchema(int index) throws IndexOutOfBoundsException {
        return (mSchemas.remove(index) != null);
    }
    
    
    /**
     * Removes the schema.
     * @param schema to remove
     * @return true if removed
     */
    public boolean removeSchema(XMLSchema schema) {
        return mSchemas.remove(schema);
    }
 
    
    /**
     * Gets the number of schemas.
     * @return the number of schemas
     */
    public int getSchemaCount() {
        return mSchemas.size();
    }
    
    
    /**
     * Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean accept(Visitor w) {
        WSDLVisitor v = morphVisitor(w);
        if (traverseParentFirst(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        
        if (!super.accept(v)) {
            return false;
        }

        if (traverseParentLast(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        return true;
    }
}
