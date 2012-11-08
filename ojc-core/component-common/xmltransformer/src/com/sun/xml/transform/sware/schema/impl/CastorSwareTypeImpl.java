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
 * @(#)CastorSwareTypeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import javax.xml.namespace.QName;

import org.exolab.castor.xml.schema.AnyType;
import org.exolab.castor.xml.schema.Schema;

import com.sun.xml.transform.sware.schema.ComponentType;
import com.sun.xml.transform.sware.schema.SwareType;
import com.sun.xml.transform.sware.schema.SwareTypeSystem;

/**
 * @see com.sun.xml.transform.sware.schema.SwareType
 * 
 * @author Jun Xu
 * @since 6.0
 * @version $Revision: 1.4 $
 */
class CastorSwareTypeImpl extends CastorSpecificComponentImpl
        implements SwareType {

    public static final QName ANYTYPE_QNAME =
        new QName(SwareTypeSystem.XSD_NS_URI, "anyType");
    private final AnyType mAnyType;
    
    /**
     * Constructs from a Castor schema object
     * 
     * @param schema a Castor schema object
     */
    CastorSwareTypeImpl(Schema schema) {
        mAnyType = new AnyType(schema);
    }
    
    /**
     * @see SwareType#isAnyType() 
     */
    public boolean isAnyType() {
        return true;
    }

    /**
     * @see SwareType#isComplexType()
     */
    public boolean isComplexType() {
        return false;
    }

    /**
     * @see SwareType#isSimpleType()
     */
    public boolean isSimpleType() {
        return false;
    }

    /**
     * @see com.sun.xml.transform.sware.schema.SchemaComponentWrapper#getOpaqueWrappedObject()
     */
    public Object getOpaqueWrappedObject() {
        return mAnyType;
    }

    public ComponentType getComponentType() {
        return ComponentType.TYPEDEF;
    }

    public QName getName() {
        return ANYTYPE_QNAME;
    }
}
