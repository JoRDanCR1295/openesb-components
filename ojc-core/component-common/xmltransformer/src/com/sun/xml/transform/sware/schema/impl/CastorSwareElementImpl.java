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
 * @(#)CastorSwareElementImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import javax.xml.namespace.QName;

import org.exolab.castor.xml.schema.ComplexType;
import org.exolab.castor.xml.schema.ElementDecl;
import org.exolab.castor.xml.schema.SimpleType;
import org.exolab.castor.xml.schema.XMLType;

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
class CastorSwareElementImpl extends CastorSwareParticleImpl
        implements SwareElement {

    private final ElementDecl mElementDecl;
    private final QName mName;
    
    private CastorSwareTypeImpl mWrappedType;
    
    /**
     * Constructs from a Castor specific element declaration object.
     * 
     * @param elemDecl a Castor specific element declaration object
     */
    CastorSwareElementImpl(ElementDecl elemDecl) {
        super(elemDecl);
        mElementDecl = elemDecl;
        if (mElementDecl.getSchema() != null) {
            mName =
                new QName(mElementDecl.getSchema().getTargetNamespace(),
                        mElementDecl.getName());
        } else {
            mName =
                new QName(mElementDecl.getName());
        }
    }

    /**
     * @see SwareElement#getType()
     */
    public SwareType getType() throws SwareSchemaException {
        if (mWrappedType != null) {
            return mWrappedType;
        }
        
        XMLType type = mElementDecl.getType();
        if (mElementDecl.getSubstitutionGroup() != null) {
            ElementDecl eDecl = mElementDecl;
            String subElemName;
            while ((subElemName = eDecl.getSubstitutionGroup()) != null
                    && eDecl.getType() == null) {
                eDecl = eDecl.getSchema().getElementDecl(subElemName);
                if (eDecl == null) {
                    throw new SwareSchemaException(
                        "Element declaration not fount: "
                            + subElemName);
                }
            }
            type = eDecl.getType();
        }
        if (type == null || type.isAnyType()) {
            mWrappedType = new CastorSwareTypeImpl(mElementDecl.getSchema());
        } else if (type.isComplexType()) {
            mWrappedType = new CastorSwareComplexTypeImpl((ComplexType) type);
        } else if (type.isSimpleType()) {
            mWrappedType = new CastorSwareSimpleTypeImpl((SimpleType) type);
        } else {
            throw new SwareSchemaException(
                    "Unrecognized type category.");
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
        return mName;
    }
}
