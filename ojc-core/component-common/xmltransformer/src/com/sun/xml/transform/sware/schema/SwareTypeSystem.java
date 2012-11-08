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
 * @(#)SwareTypeSystem.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema;

import java.util.Collection;

import javax.xml.namespace.QName;

import com.sun.xml.transform.sware.schema.impl.ProtectedFactory;

/**
 * The instance of this class represents an XML schema type system.  A schema
 * type system may contain multiple schema objects and it provides overall
 * support for facilitating schema component lookup across all schemas it
 * contains.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public interface SwareTypeSystem extends ImplSpecificComponent {

    /**
     * XML schema instance namespace.
     */
    public static final String XSI_NS_URI =
        "http://www.w3.org/2001/XMLSchema-instance";
    
    /**
     * XML schema namespace. 
     */
    public static final String XSD_NS_URI =
        "http://www.w3.org/2001/XMLSchema";

    /**
     * Adds a schema object to the schema type system.
     * 
     * @param swareSchema a schema object
     * @throws SwareSchemaException invalid schema exception
     */
    public void addSchema(SwareSchema swareSchema)
        throws SwareSchemaException;
    
    /**
     * Looks up an element declaration using a qualified name.
     * 
     * @param qName the qualified name
     * @return an element declaration that matches the qualified name
     * @throws SwareSchemaException invalid schema exception
     */
    public SwareElement findElement(QName qName)
        throws SwareSchemaException;
    
    /**
     * Looks up an XML type definition using a qualified name.
     * 
     * @param qName the qualified name
     * @return an XML type definition that matches the qualified name
     * @throws SwareSchemaException invalid schema exception
     */
    public SwareType findType(QName qName)
        throws SwareSchemaException;
    
    /**
     * Looks up a collection of element declarations that are substitutes of the
     * element declaration specified by a qualified name.
     *  
     * @param qName the qualified name used to look up substitues
     * @return a collection of qualified names that represent substitution
     *         element declarations
     * @throws SwareSchemaException invalid schema exception
     */
    public Collection<QName> getSubstitutionSet(QName qName)
        throws SwareSchemaException;

    /**
     * Looks up a flattened complex type definition using a complex type
     * definition.
     *  
     * @param swareComplexType the complex type definition used for lookup
     * @return a flattened complex type definition if the complex type definiton
     *         is found in the type system, otherwise <code>null</code>
     * @throws SwareSchemaException
     */
    public SwareFlatComplexType getFlatComplexType(
            SwareComplexType swareComplexType)
        throws SwareSchemaException;
    
    /**
     * A convenient factory class for bootstraping a schema type system based
     * on a specific implementation type.
     */
    public static final class Factory {
        
        /**
         * Creates an empty schema type system instance based on the
         * implementation type required.
         * 
         * @param implType an implementation type
         * @return a schema type system instance
         */
        public static SwareTypeSystem newSchemaTypeSystem(
                ImplementationType implType) {

            return ProtectedFactory.newSwareTypeSystem(implType);
        }
        
        /**
         * Creates a sware type system instance based on the
         * implementation type required and an implementation specific type
         * system.
         * 
         * @param implType the implementation type
         * @param implSpecificTypeSystem the implementation specfic
         *                               type system instance 
         * @return a sware type system instance
         */
        public static SwareTypeSystem newSchemaTypeSystem(
                ImplementationType implType, Object implSpecificTypeSystem) {

            return ProtectedFactory.newSwareTypeSystem(
                    implType, implSpecificTypeSystem);
        }
    }
}
