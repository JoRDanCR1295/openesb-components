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
 * @(#)CastorSwareSimpleTypeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import javax.xml.namespace.QName;

import org.exolab.castor.xml.schema.SimpleType;

import com.sun.xml.transform.sware.schema.ComponentType;
import com.sun.xml.transform.sware.schema.SwareTypeSystem;

/**
 * @see com.sun.xml.transform.sware.schema.SwareType
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
class CastorSwareSimpleTypeImpl extends CastorSwareTypeImpl {

    private final SimpleType mSimpleType;
    private final QName mName;
    
    /**
     * Constructs from a Castot specific simple type defintion object.
     * 
     * @param simpleType a Castot specific simple type defintion object
     */
    CastorSwareSimpleTypeImpl(SimpleType simpleType) {
        super(simpleType.getSchema());
        
        mSimpleType = simpleType;
        if (mSimpleType.getName() != null) {
            if (simpleType.getSchema() != null) {
                mName =
                    new QName(mSimpleType.getSchema().getTargetNamespace(),
                        mSimpleType.getName());
            } else {
                if (mSimpleType.isBuiltInType()) {
                    mName =
                        new QName(SwareTypeSystem.XSD_NS_URI,
                                mSimpleType.getName());
                } else {
                    mName =
                        new QName(mSimpleType.getName());
                }
            }
        } else {
            mName = null;
        }
    }

    /**
     * @see com.sun.xml.transform.sware.schema.SchemaComponentWrapper#getOpaqueWrappedObject()
     */
    @Override
    public Object getOpaqueWrappedObject() {
        return mSimpleType;
    }

    /**
     * @see com.sun.xml.transform.sware.schema.SwareType#isAnyType()
     */
    @Override
    public boolean isAnyType() {
        return false;
    }

    /**
     * @see com.sun.xml.transform.sware.schema.SwareType#isComplexType()
     */
    @Override
    public boolean isComplexType() {
        return false;
    }

    /**
     * @see com.sun.xml.transform.sware.schema.SwareType#isSimpleType()
     */
    @Override
    public boolean isSimpleType() {
        return true;
    }

    @Override
    public ComponentType getComponentType() {
        return ComponentType.SIMPLETYPEDEF;
    }

    @Override
    public QName getName() {
        return mName;
    }
}
