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
 * @(#)InComingEventKeyImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;


/**
 * incoming event key implementation
 *
 * @author Sun Microsystems
 */
public class InComingEventKeyImpl {
    // TODO We can do away with this class and we should.
    // for now it is a good transition class. Once things get working we can remove this class
    // usage completely.

    private int mType;
    private InComingEventModel mModel;

    /**
     * Creates a new InComingEventKeyImpl object.
     *
     * @param model incoming event model
     * @param type event type
     */
    public InComingEventKeyImpl(InComingEventModel model, int type) {
        mModel = model;
        mType = type;
    }

    /**
     * Creates a new InComingEventKeyImpl object.
     */
    protected InComingEventKeyImpl() {
    }

    /**
     * gets event model
     *
     * @return InComingEventModel incoming event model
     */
    public InComingEventModel getEventModel() {
        return mModel;
    }

    /**
     * gets BPEL process
     *
     * @return RBPELProcess runtime bpel process
     */
    public RBPELProcess getBPELProcess() {
        // TODO REvisit this Key class. this API is exposed for the abstraction 
        // that is required for the subClass.
        return mModel.getBPELProcess();
    }

    /**
     * gets event type
     *
     * @return int event type
     */
    public int getType() {
        return mType;
    }
}
