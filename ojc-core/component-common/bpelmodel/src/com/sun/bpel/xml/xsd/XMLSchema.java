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
 * @(#)XMLSchema.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.xsd;

import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument.Schema;

import com.sun.wsdl4j.ext.DeferredActionAccepter;

/**
 * Container for an XML schema.
 * 
 * @author Jun Xu
 * @version $Revision: 1.3 $
 */
public interface XMLSchema extends DeferredActionAccepter {
    
    /** The XML Schema URI. */
    public static final String URI = "http://www.w3.org/2001/XMLSchema";
    
    /** The Tag for the schema element */
    public static final String TAG = "schema";
    
    /**
     * Gets the URI from which the schema is loaded.
     * 
     * @return The URI
     */
    public String getURI();
    
    /**
     * Gets the target namespace of the schema.  If no namespace,
     * <code>null</code> may be returned.
     * 
     * @return The target namespace
     */
    public String getTargetNamespace();
    
    /**
     * Gets the schema. The return value might be <code>null</code> if
     * <code>BPELProcess</code>'s <code>resolveAll</code> has been called (to
     * free up heap).
     * 
     * @return The schema
     */
    public Schema getSchema();
}
