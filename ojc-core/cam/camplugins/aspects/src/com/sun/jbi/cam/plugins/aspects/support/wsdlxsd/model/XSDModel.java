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
 * @(#)XSDModel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model;

import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlException;

import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.parsers.XSDParser;

/**
 * @author Graj
 *
 */
public interface XSDModel {

    /**
     * @return Returns the xsdParser.
     */
    public abstract XSDParser getXsdParser();

    /**
     * Compiles a list of schema objects and returns a finite set of
     * XMLSchema component definitions
     * 
     * @param importMap
     * @param typeNodes
     * @param namespaces
     * @return a SchemaTypeSystem which is a finite set of XMLSchema component definitions
     * @throws XmlException
     */
    public abstract SchemaTypeSystem populate(Map importMap, List typeNodes,
            Map namespaces) throws XmlException;

    /**
     * @return a SchemaTypeSystem which is a finite set of XMLSchema component definitions
     */
    public abstract SchemaTypeSystem getSchemaTypeSystem();

    /**
     * Returns the builtin type system. This SchemaTypeSystem contains
     * only the 46 builtin types defined by the XML Schema specification. 
     * @return a SchemaTypeSystem of the builtin type system 
     */
    public abstract SchemaTypeSystem getBuiltInTypeSystem();

    /**
     * Get the SchemaType associated with this elementQName
     * @param elementQName
     * @return a SchemaType object of the elementName
     */
    public abstract SchemaType getExpandedSchemaType(QName elementQName);
    
    /**
     * Retrieves all the Schema Types contained in this Type System
     * @return a SchemaType array
     */
    public abstract SchemaType[] retrieveAllSchemaTypes();

    /**
     * Retrive the SchemaType associated with this elementQName
     * @param elementQName
     * @return a SchemaType object of the elementName
     */
    public abstract SchemaType retrieveSchemaType(QName elementQName);

    /**
     * Retrive the SchemaType associated with this elementName
     * @param elementName
     * @return a SchemaType object of the elementName
     */
    public abstract SchemaType retrieveSchemaType(String elementName);

    /**
     * 
     * @param type
     * @return a SchemaType
     */
    public abstract SchemaType getDataStructure(SchemaType type);

}
