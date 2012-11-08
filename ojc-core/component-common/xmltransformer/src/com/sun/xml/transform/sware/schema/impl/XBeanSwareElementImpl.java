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
 * @(#)XBeanSwareElementImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaLocalElement;
import org.apache.xmlbeans.SchemaParticle;
import org.apache.xmlbeans.SchemaType;

import com.sun.xml.transform.sware.schema.ComponentType;
import com.sun.xml.transform.sware.schema.SwareElement;
import com.sun.xml.transform.sware.schema.SwareSchemaException;
import com.sun.xml.transform.sware.schema.SwareType;

/**
 * @see com.sun.xml.transform.sware.schema.SwareElement
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
class XBeanSwareElementImpl extends XBeanSwareParticleImpl
        implements SwareElement {

    private final SchemaLocalElement mElementDecl;
    
    private XBeanSwareTypeImpl mWrappedType;
    
    /**
     * Constructs from an XmlBeans specific element declaration object.
     * 
     * @param elemDecl an XmlBeans specific element declaration object
     */
    XBeanSwareElementImpl(SchemaLocalElement elemDecl) {
        super((SchemaParticle) elemDecl);
        mElementDecl = elemDecl;
    }

    /**
     * @see SwareElement#getType()
     */
    public SwareType getType() throws SwareSchemaException {
        if (mWrappedType != null) {
            return mWrappedType;
        }
        
        SchemaType type = mElementDecl.getType();
        if ((mElementDecl instanceof SchemaGlobalElement)
                && ((SchemaGlobalElement) mElementDecl).substitutionGroup()
                        != null && mElementDecl.getType() == null) {
            SchemaGlobalElement eDecl = (SchemaGlobalElement) mElementDecl;
            SchemaGlobalElement prev = eDecl;
            while (eDecl != null && eDecl.getType() == null) {
                prev = eDecl;
                eDecl = eDecl.substitutionGroup();
            }
            type = prev.getType();
        }
        if (type == null || type.isURType() && !type.isSimpleType()) {
            mWrappedType = new XBeanSwareTypeImpl();
        } else if (type.isSimpleType()) {
            mWrappedType = new XBeanSwareSimpleTypeImpl(type);
        } else {
            mWrappedType = new XBeanSwareComplexTypeImpl(type);
        }
        return mWrappedType;
    }

    /**
     * @see com.sun.xml.transform.sware.schema.SchemaComponentWrapper#getOpaqueWrappedObject()
     */
    public Object getOpaqueWrappedObject() {
        return mElementDecl;
    }

    /**
     * @see com.sun.xml.transform.sware.schema.SwareParticle#getParticleType()
     */
    @Override
    public ParticleType getParticleType() {
        return ParticleType.ELEMENTDECL;
    }

    public ComponentType getComponentType() {
        return ComponentType.ELEMENTDECL;
    }

    public QName getName() {
        return mElementDecl.getName();
    }
}
