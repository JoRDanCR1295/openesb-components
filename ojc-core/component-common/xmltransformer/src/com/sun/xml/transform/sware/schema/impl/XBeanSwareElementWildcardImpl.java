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
 * @(#)CastorSwareElementWildcardImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaParticle;

import com.sun.xml.transform.sware.schema.ComponentType;
import com.sun.xml.transform.sware.schema.SwareElementWildcard;

/**
 * @see com.sun.xml.transform.sware.schema.SwareElementWildcard
 * 
 * @author Jun Xu
 * @version $Revision: 1.1 $
 */
class XBeanSwareElementWildcardImpl extends XBeanSwareParticleImpl
        implements SwareElementWildcard {

    private final SchemaParticle mWildcard;
    
    /**
     * Constructs from an xmlbeans specific wildcard declaration object.
     * @param wildcard an xmlbeans specific wildcard declaration object
     */
    XBeanSwareElementWildcardImpl(SchemaParticle wildcard) {
        super(wildcard);
        
        if (wildcard.getParticleType() != SchemaParticle.WILDCARD) {
            throw new IllegalArgumentException(
                    "Particle is not a wildcard. '"
                    + (wildcard.getName() != null ? wildcard.getName() :
                        wildcard.acceptedStartNames() != null ?
                                wildcard.acceptedStartNames() :
                                    wildcard.toString()));
        }
        mWildcard = wildcard;
    }
    
    /**
     * @see com.sun.xml.transform.sware.schema.SwareParticle#getParticleType()
     */
    @Override
    public ParticleType getParticleType() {
        return ParticleType.WILDCARD;
    }

    /**
     * @see com.sun.xml.transform.sware.schema.SchemaComponentWrapper#getOpaqueWrappedObject()
     */
    public Object getOpaqueWrappedObject() {
        return mWildcard;
    }

    public ComponentType getComponentType() {
        return ComponentType.WILDCARD;
    }

    public QName getName() {
        return null;
    }
}
