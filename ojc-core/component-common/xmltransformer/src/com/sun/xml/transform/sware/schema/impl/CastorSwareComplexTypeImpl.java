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
 * @(#)CastorSwareComplexTypeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import javax.xml.namespace.QName;

import org.exolab.castor.xml.schema.ComplexType;
import org.exolab.castor.xml.schema.ElementDecl;

import com.sun.xml.transform.sware.schema.ComponentType;
import com.sun.xml.transform.sware.schema.SwareComplexType;
import com.sun.xml.transform.sware.schema.SwareElement;

/**
 * @see com.sun.xml.transform.sware.schema.SwareComplexType
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
class CastorSwareComplexTypeImpl extends CastorSwareTypeImpl
        implements SwareComplexType{

    private final ComplexType mComplexType;
    private final QName mName;
    
    /**
     * Constructs from a Castor specific complex type definition object.
     * 
     * @param cType a Castor specific complex type definition object
     */
    CastorSwareComplexTypeImpl(ComplexType cType) {
        super(cType.getSchema());
        
        mComplexType = cType;
        if (mComplexType.getName() != null) {
            if (mComplexType.getSchema() != null) {
                mName =
                    new QName(mComplexType.getSchema().getTargetNamespace(),
                            mComplexType.getName());
            } else {
                mName =
                    new QName(mComplexType.getName());
            }
        } else {
            mName = null;
        }
    }

    /**
     * @see SwareType#isAnyType()
     */
    @Override
    public boolean isAnyType() {
        return false;
    }

    /**
     * @see SwareType#isComplexType()
     */
    @Override
    public boolean isComplexType() {
        return true;
    }

    /**
     * @see SwareType#isSimpleType()
     */
    @Override
    public boolean isSimpleType() {
        return false;
    }

    /**
     * @see com.sun.xml.transform.sware.schema.SchemaComponentWrapper#getOpaqueWrappedObject()
     */
    @Override
    public Object getOpaqueWrappedObject() {
        return mComplexType;
    }

    /**
     * @see SwareComplexType#isSimpleContent()
     */
    public boolean isSimpleContent() {
        return mComplexType.isSimpleContent();
    }

    /**
     * @see SwareComplexType#isComplexContent()
     */
    public boolean isComplexContent() {
        return mComplexType.isComplexContent();
    }

    @Override
    public ComponentType getComponentType() {
        return ComponentType.COMPLEXTYPEDEF;
    }

    @Override
    public QName getName() {
        return mName;
    }

    public SwareElement getContainerElement() {
        if (mComplexType.getParent() instanceof ElementDecl) {
            return new CastorSwareElementImpl(
                    (ElementDecl) mComplexType.getParent());
        }
        return null;
    }
}
