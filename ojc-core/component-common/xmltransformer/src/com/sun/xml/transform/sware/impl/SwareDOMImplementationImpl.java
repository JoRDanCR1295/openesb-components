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
 * @(#)SwareDOMImplementationImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.impl;

import java.lang.ref.SoftReference;
import java.util.HashMap;
import java.util.Map;

import javax.xml.transform.Transformer;

import org.exolab.castor.xml.schema.Schema;
import org.w3c.dom.DOMException;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;

import com.sun.xml.transform.sware.InvalidSchemaException;
import com.sun.xml.transform.sware.SwareDOMImplementation;
import com.sun.xml.transform.sware.schema.SwareSchema;
import com.sun.xml.transform.sware.schema.SwareSchemaException;
import com.sun.xml.transform.sware.schema.SwareTypeSystem;

/**
 * This class provides an implementation of the SwareDOMImplementation
 * interface.
 *  
 * @author Jun Xu
 * @since 6.0
 * @version $Revision: 1.4 $
 */
public class SwareDOMImplementationImpl implements SwareDOMImplementation {
    
    private final DOMImplementation mDOMImpl;
    private Map<SwareSchema, SoftReference<SwareTypeSystem>> mTSWriteCache =
        new HashMap<SwareSchema, SoftReference<SwareTypeSystem>>();
    private Map<SwareSchema, SoftReference<SwareTypeSystem>> mTSReadCache =
        new HashMap<SwareSchema, SoftReference<SwareTypeSystem>>();

    
    /**
     * Constructs from an instance of DOMImplementation.
     * 
     * @param domImpl an instance of DOMImplementation
     */
    public SwareDOMImplementationImpl(DOMImplementation domImpl) {
        mDOMImpl = domImpl;
    }

    /**
     * @see SwareDOMImplementation#createDocument(String, String, Schema, Policies)
     */
    public Document createDocument(String namespaceURI, String qualifiedName,
            SwareSchema swareSchema, Policies policies) throws DOMException {
        throw new UnsupportedOperationException(
                "This method has not been implemented.");
    }

    /**
     * @see SwareDOMImplementation#createReorderTransformer(Schema, Policies)
     */
    public Transformer createReorderTransformer(SwareSchema swareSchema,
            Policies policies) throws InvalidSchemaException {
        SoftReference<SwareTypeSystem> ref = mTSReadCache.get(swareSchema);
        SwareTypeSystem ts;
        if (ref == null || (ts = ref.get()) == null) {
            ts = SwareTypeSystem.Factory.newSchemaTypeSystem(
                    swareSchema.getImplementationType());
            try {
                ts.addSchema(swareSchema);
            } catch (SwareSchemaException e) {
                throw new InvalidSchemaException("Invalid schema.", e);
            }
            synchronized (mTSWriteCache) {
                if (!mTSWriteCache.containsKey(swareSchema)) {
                    mTSWriteCache.put(swareSchema,
                            new SoftReference<SwareTypeSystem>(ts));
                    mTSReadCache =
                        new HashMap<SwareSchema,
                            SoftReference<SwareTypeSystem>>(mTSWriteCache);
                }
            }
        }
        return new ReorderTransformerImpl(mDOMImpl, ts, policies);
    }

    /**
     * @see SwareDOMImplementation#createDocument(String, String, SwareTypeSystem, SwareDOMImplementation.Policies)
     */
    public Document createDocument(String namespaceURI, String qualifiedName,
            SwareTypeSystem swareTypeSystem, Policies policies)
            throws DOMException {
        throw new UnsupportedOperationException(
            "This method has not been implemented.");
    }

    /**
     * @see SwareDOMImplementation#createReorderTransformer(SwareTypeSystem, SwareDOMImplementation.Policies)
     */
    public Transformer createReorderTransformer(
            SwareTypeSystem swareTypeSystem, Policies policies)
            throws InvalidSchemaException {
        return new ReorderTransformerImpl(mDOMImpl, swareTypeSystem, policies);
    }
}
