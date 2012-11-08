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
 * @(#)DeferredActionRegistry.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl4j.ext;

import java.util.Collection;
import java.util.LinkedHashSet;

import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument.Schema;

/**
 * This class defines a registry which can be used to register information for
 * deferred actions (post loading actions).  This mechanism is currently used
 * to build single schema type loader and perform actions after the single type
 * system/loader is built and passed to type system loader holders.   
 * 
 * @author Jun Xu
 */
public class DeferredActionRegistry {
    
    private Collection<Schema> _schemaDocCollector = new LinkedHashSet<Schema>();
    private Collection<SchemaTypeLoaderHolder> _typeLoaderHolderCollector =
        new LinkedHashSet<SchemaTypeLoaderHolder>();
    private Collection<DeferredActionAccepter> _actionAccepterCollector =
        new LinkedHashSet<DeferredActionAccepter>();
    
    /**
     * Gets the schema document collector, which collects all schema documents
     * encountered during the loading process.
     * 
     * @return The schema document collector
     */
    public Collection<Schema> getSchemaDocumentCollector() {
        return _schemaDocCollector;
    }
    
    /**
     * Gets the schema type loader holder collector, which can be used to
     * collect all schema type loader holders encountered during loading
     * process.
     * 
     * @return The schema type loader holder collector.
     */
    public Collection<SchemaTypeLoaderHolder> getTypeLoaderHolderCollector() {
        return _typeLoaderHolderCollector;
    }
    
    /**
     * Gets the deferred action accepter collector, which can be used to
     * collect all deferred action acceptors encountered during loading
     * process.
     *  
     * @return The deferred action accepter collector
     */
    public Collection<DeferredActionAccepter> getActionAccepterCollector() {
        return _actionAccepterCollector;
    }
}
