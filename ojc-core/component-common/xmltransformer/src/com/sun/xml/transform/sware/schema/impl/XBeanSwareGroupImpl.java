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
 * @(#)XBeanSwareGroupImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaParticle;

import com.sun.xml.transform.sware.schema.ComponentType;
import com.sun.xml.transform.sware.schema.SwareGroup;
import com.sun.xml.transform.sware.schema.SwareParticle;
import com.sun.xml.transform.sware.schema.SwareSchemaException;

/**
 * @see com.sun.xml.transform.sware.schema.SwareGroup
 * 
 * @author Jun Xu
 * @version $Revision: 1.1 $ 
 */
class XBeanSwareGroupImpl extends XBeanSwareParticleImpl
        implements SwareGroup {

    private final SchemaParticle mGroup;
    private final Compositor mCompositor;
    
    private SwareParticle[] mParticles;
    private int mParticleCount = -1;
    
    public Object mMonitor1 = new Object();
    public Object mMonitor2 = new Object();
    
    /**
     * Constructs from a Castor specific group definition object.
     * 
     * @param group a Castor specific group definition object
     */
    XBeanSwareGroupImpl(SchemaParticle group) {
        super(group);
        if (group.getParticleType() == SchemaParticle.ALL) {
            mCompositor = Compositor.ALL;
        } else if (group.getParticleType() == SchemaParticle.SEQUENCE) {
            mCompositor = Compositor.SEQUENCE;
        } else if (group.getParticleType() == SchemaParticle.CHOICE) {
            mCompositor = Compositor.CHOICE;
        } else {
            throw new IllegalArgumentException(
                    "Particle is not a group. '"
                    + (group.getName() != null ? group.getName() :
                        group.acceptedStartNames() != null ?
                                group.acceptedStartNames() :
                                    group.toString()));
        }
        mGroup = group;
        mParticleCount = group.countOfParticleChild();
    }
    
    /**
     * @see SwareGroup#getCompositor()
     */
    public Compositor getCompositor() {
        return mCompositor;
    }

    /**
     * @see SwareGroup#getParticles()
     */
    public SwareParticle[] getParticles() {
        if (mParticles != null) {
            return mParticles;
        }
        SwareParticle[] swareParts;
        //To avoid compiler merging sync blocks and doing reorder, use
        //two different monitors
        synchronized (mMonitor1) {
            swareParts =
                new SwareParticle[mGroup.countOfParticleChild()];
            for (int i = 0; i < swareParts.length; i++) {
                swareParts[i] =
                    (SwareParticle) XBeanUtil.wrapParticle(
                            mGroup.getParticleChild(i));
            }
        }
        synchronized (mMonitor2) {
            if (mParticles != null) {
                return mParticles;
            }
            mParticles = swareParts;
        }
        return mParticles;
    }

    /**
     * @see com.sun.xml.transform.sware.schema.SchemaComponentWrapper#getOpaqueWrappedObject()
     */
    public Object getOpaqueWrappedObject() {
        return mGroup;
    }

    /**
     * @see SwareParticle#getParticleType()
     */
    @Override
    public ParticleType getParticleType() {
        return ParticleType.GROUP;
    }

    /**
     * @see SwareGroup#isReference()
     */
    public boolean isReference() {
        return false;
    }

    /**
     * @see SwareGroup#getReference()
     */
    public SwareGroup getReference() {
        return null;
    }

    /**
     * @see SwareGroup#getParticleCount()
     */
    public int getParticleCount() throws SwareSchemaException {
        return mParticleCount;
    }

    /**
     * @see SwareGroup#getParticle(int)
     */
    public SwareParticle getParticle(int i) throws SwareSchemaException {
        if (i < 0 || i >= mParticleCount) {
            throw new ArrayIndexOutOfBoundsException(i);
        }
        getParticles();
        return mParticles[i];
    }

    public ComponentType getComponentType() {
        return ComponentType.MODELGROUP;
    }

    public QName getName() {
        return mGroup.getName();
    }
}
