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
 * @(#)CastorSwareGroupImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.schema.impl;

import javax.xml.namespace.QName;

import org.exolab.castor.xml.schema.Group;
import org.exolab.castor.xml.schema.ModelGroup;
import org.exolab.castor.xml.schema.Order;

import com.sun.xml.transform.sware.schema.ComponentType;
import com.sun.xml.transform.sware.schema.SwareGroup;
import com.sun.xml.transform.sware.schema.SwareParticle;
import com.sun.xml.transform.sware.schema.SwareSchemaException;

/**
 * @see com.sun.xml.transform.sware.schema.SwareGroup
 * 
 * @author Jun Xu
 * @since 6.0
 * @version $Revision: 1.4 $
 */
class CastorSwareGroupImpl extends CastorSwareParticleImpl
        implements SwareGroup {

    private final Group mGroup;
    
    private SwareGroup mReference;
    private SwareParticle[] mParticles;
    private final QName mName;
    private final Compositor mCompositor;
    private final int mParticleCount;
    
    /**
     * Constructs from a Castor specific group definition object.
     * 
     * @param group a Castor specific group definition object
     */
    CastorSwareGroupImpl(Group group) {
        super(group);
        mGroup = group;
        if (mGroup instanceof ModelGroup) {
            ModelGroup mgroup = (ModelGroup) mGroup;
            if (mgroup.getSchema() != null) {
                mName =
                    new QName(mgroup.getSchema().getTargetNamespace(),
                            mgroup.getName());
            } else {
                mName =
                    new QName(mgroup.getName());
            }
        } else {
            mName = null;
        }
        if (mGroup.getOrder().equals(Order.all)) {
            mCompositor = Compositor.ALL;
        } else if (mGroup.getOrder().equals(Order.choice)) {
            mCompositor = Compositor.CHOICE;
        } else {
            mCompositor = Compositor.SEQUENCE;
        }
        mParticleCount = mGroup.getParticleCount();
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
    public synchronized SwareParticle[] getParticles() {
        if (mParticles != null) {
            return mParticles;
        }
        SwareParticle[] swareParts =
            new SwareParticle[mGroup.getParticleCount()];
        for (int i = 0; i < swareParts.length; i++) {
            swareParts[i] =
                (SwareParticle) CastorUtil.wrapStructure(mGroup.getParticle(i));
        }
        mParticles = swareParts;
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
        return (mGroup instanceof ModelGroup)
            && ((ModelGroup) mGroup).isReference();
    }

    /**
     * @see SwareGroup#getReference()
     */
    public SwareGroup getReference() {
        if (!(mGroup instanceof ModelGroup)) {
            return null;
        }
        synchronized (this) {
            if (mReference != null) {
                return mReference;
            }
            mReference = new CastorSwareGroupImpl(
                    ((ModelGroup) mGroup).getReference());
            return mReference;
        }
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
        if (i < 0 || i >= mGroup.getParticleCount()) {
            throw new ArrayIndexOutOfBoundsException(i);
        }
        return (SwareParticle) CastorUtil.wrapStructure(mGroup.getParticle(i));
    }

    public ComponentType getComponentType() {
        return ComponentType.MODELGROUP;
    }

    public QName getName() {
        return mName;
    }
}
