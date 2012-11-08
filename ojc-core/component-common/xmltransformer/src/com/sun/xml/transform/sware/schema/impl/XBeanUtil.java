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
 * @(#)XBeanUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import org.apache.xmlbeans.SchemaComponent;
import org.apache.xmlbeans.SchemaLocalElement;
import org.apache.xmlbeans.SchemaParticle;

import com.sun.xml.transform.sware.schema.SwareParticle;
import com.sun.xml.transform.sware.schema.SwareSchemaComponent;

/**
 * Castor specific utilities.
 * 
 * @author Jun Xu
 * @version $Revision: 1.1 $
 */
public class XBeanUtil {
    
    /**
     * Wraps an XmlBeans schema particle into a Sware schema component.
     * 
     * @param xbeanParticle the XmlBeans schema component
     * @return the wrapped Sware schema component
     * @throws UnsupportedOperationException thrown if the wrapping of the
     *          particle of a specific type is not supported
     */
    public static SwareParticle wrapParticle(
            SchemaParticle xbeanParticle)
    throws UnsupportedOperationException {
        if (xbeanParticle == null) {
            return null;
        }
        switch (xbeanParticle.getParticleType()) {
        case SchemaParticle.ALL:
        case SchemaParticle.SEQUENCE:
        case SchemaParticle.CHOICE:
            return new XBeanSwareGroupImpl(xbeanParticle);
        case SchemaParticle.ELEMENT:
            return new XBeanSwareElementImpl(
                    (SchemaLocalElement) xbeanParticle);
        case SchemaParticle.WILDCARD:
            return new XBeanSwareElementWildcardImpl(xbeanParticle);
        default:
            //This is not possible
            throw new UnsupportedOperationException(
                    "Wrapping particle of type ["
                    + xbeanParticle.getParticleType()
                    + "] is not supported.");
        }
    }
}
