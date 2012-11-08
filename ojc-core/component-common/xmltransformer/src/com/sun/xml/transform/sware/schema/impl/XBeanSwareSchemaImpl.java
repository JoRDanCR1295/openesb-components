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
 * @(#)XBeanSwareSchemaImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.impl.xb.xsdschema.SchemaDocument.Schema;

import com.sun.xml.transform.sware.schema.ComponentType;
import com.sun.xml.transform.sware.schema.SwareSchema;
import com.sun.xml.transform.sware.schema.SwareSchemaComponent;

/**
 * @see com.sun.xml.transform.sware.schema.SwareSchema
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
class XBeanSwareSchemaImpl extends XBeanSpecificComponentImpl
        implements SwareSchema {

    private final Schema mSchema;
    
    /**
     * Constructs from an XmlBeans specific schema object.
     * 
     * @param xbeanSchema an XmlBeans specific schema object
     */
    public XBeanSwareSchemaImpl(Object xbeanSchema) {
        mSchema = (Schema) xbeanSchema;
    }

    /**
     * @see com.sun.xml.transform.sware.schema.SchemaComponentWrapper#getOpaqueWrappedObject()
     */
    public Object getOpaqueWrappedObject() {
        return mSchema;
    }

    public ComponentType getComponentType() {
        return ComponentType.SCHEMA;
    }

    public QName getName() {
        return null;
    }

    public SwareSchemaComponent getOwner() {
        return null;
    }
}
