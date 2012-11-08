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
 * @(#)XBeanSwareTypeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.XmlBeans;

import com.sun.xml.transform.sware.schema.ComponentType;
import com.sun.xml.transform.sware.schema.SwareSchemaComponent;
import com.sun.xml.transform.sware.schema.SwareType;

/**
 * @see com.sun.xml.transform.sware.schema.SwareType
 * 
 * @author Jun Xu
 * @version $Revision: 1.1 $
 */
public class XBeanSwareTypeImpl extends XBeanSpecificComponentImpl
implements SwareType {
    
    public static final QName ANYTYPE_QNAME =
        new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "anyType");

    public boolean isAnyType() {
        return true;
    }

    public boolean isComplexType() {
        return false;
    }

    public boolean isSimpleType() {
        return false;
    }

    public Object getOpaqueWrappedObject() {
        return XmlBeans.getBuiltinTypeSystem().findType(ANYTYPE_QNAME);
    }

    public ComponentType getComponentType() {
        return ComponentType.TYPEDEF;
    }

    public QName getName() {
        return ANYTYPE_QNAME;
    }

    public SwareSchemaComponent getOwner() {
        return null;
    }
}
