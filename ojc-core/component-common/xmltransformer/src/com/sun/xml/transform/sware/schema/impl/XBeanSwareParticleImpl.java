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
 * @(#)XBeanSwareParticleImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import java.math.BigInteger;

import org.apache.xmlbeans.SchemaParticle;

import com.sun.xml.transform.sware.schema.SwareParticle;

/**
 * @see com.sun.xml.transform.sware.schema.SwareParticle
 * 
 * @author Jun Xu
 * @version $Revision: 1.1 $
 */
abstract class XBeanSwareParticleImpl extends XBeanSpecificComponentImpl
        implements SwareParticle {

    private final SchemaParticle mParticle;
    
    /**
     * Constructs from an XmlBeans specific particle object.
     * 
     * @param part an XmlBeans specific particle object
     */
    XBeanSwareParticleImpl(SchemaParticle part) {
        if (part == null) {
            throw new IllegalArgumentException(
                    "no particle.");
        }
        mParticle = part;
    }
    
    /**
     * @see SwareParticle#getParticleType()
     */
    public abstract ParticleType getParticleType();
    
    /**
     * @see SwareParticle#getMinOccurs()
     */
    public BigInteger getMinOccurs() {
        return mParticle.getMinOccurs();
    }
    
    /**
     * @see SwareParticle#getMaxOccurs()
     */
    public BigInteger getMaxOccurs() {
        return mParticle.getMaxOccurs();
    }
}
