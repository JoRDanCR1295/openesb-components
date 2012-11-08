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
 * @(#)FakeXBeanSchemaGroup.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.xml.transform.sware.schema.impl;

import java.math.BigInteger;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.QNameSet;
import org.apache.xmlbeans.SchemaParticle;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.XmlAnySimpleType;

/**
 * The instance of this class is used to mimic a SchemaParticle with
 * particle type of SEQUENCE and minOccurs = maxOccurs = 1.  It represents
 * a useless group in XSD terminology.  The reason for having this is
 * that XmlBeans always removes useless groups.  But for easy process, we still
 * need this level.
 * 
 * @author Jun Xu
 * @version $Revision: 1.1 $
 */
public class FakeXBeanSchemaGroup implements SchemaParticle {

    // The wrapped element or wildcard particle
    private final SchemaParticle mWrappedParticle;
    
    public FakeXBeanSchemaGroup(SchemaParticle part) {
        mWrappedParticle = part;
    }
    
    public QNameSet acceptedStartNames() {
        return mWrappedParticle.acceptedStartNames();
    }

    public boolean canStartWithElement(QName arg0) {
        return mWrappedParticle.canStartWithElement(arg0);
    }

    public int countOfParticleChild() {
        return 1;
    }

    public String getDefaultText() {
        return null;
    }

    public XmlAnySimpleType getDefaultValue() {
        return null;
    }

    public int getIntMaxOccurs() {
        return 1;
    }

    public int getIntMinOccurs() {
        return 1;
    }

    public BigInteger getMaxOccurs() {
        return BigInteger.ONE;
    }

    public BigInteger getMinOccurs() {
        return BigInteger.ONE;
    }

    public QName getName() {
        return null;
    }

    public SchemaParticle getParticleChild(int arg0) {
        if (arg0 != 0) {
            throw new ArrayIndexOutOfBoundsException(arg0);
        }
        return mWrappedParticle;
    }

    public SchemaParticle[] getParticleChildren() {
        return new SchemaParticle[]{mWrappedParticle};
    }

    public int getParticleType() {
        return SchemaParticle.SEQUENCE;
    }

    public SchemaType getType() {
        return null;
    }

    public int getWildcardProcess() {
        return 0;
    }

    public QNameSet getWildcardSet() {
        return null;
    }

    public boolean isDefault() {
        return false;
    }

    public boolean isFixed() {
        return false;
    }

    public boolean isNillable() {
        return false;
    }

    public boolean isSingleton() {
        return true;
    }

    public boolean isSkippable() {
        return mWrappedParticle.isSkippable();
    }
}
