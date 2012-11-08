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
 * @(#)InComingEventModelImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;


/**
 * incoming event model implementation
 *
 * @author Sun Microsystems
 */
public class InComingEventModelImpl implements InComingEventModel {
    private RBPELProcess mProc;
    private RStartElement mStartElement;
    private String mOperPattern;

    /**
     * Creates a new InComingEventModelImpl object.
     *
     * @param proc runtime bpel process
     * @param sa start activity
     * @param operPattern operation pattern
     */
    public InComingEventModelImpl(RBPELProcess proc, RStartElement sa,
        String operPattern) {
        mProc = proc;
        mStartElement = sa;
        mOperPattern = operPattern;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel#getBPELProcess()
     */
    public RBPELProcess getBPELProcess() {
        return mProc;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel#getOperPattern()
     */
    public String getOperPattern() {
        return mOperPattern;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel#getStartElement()
     */
    public RStartElement getStartElement() {
        return mStartElement;
    }

    public String toString() {
        return "IMA: " + mProc.getTargetNamespace() + ":"
                + mStartElement.getRPartner().getName() + " PortType: "
                + mStartElement.getRPortType() + " Operation: "
                + mStartElement.getWSDLOperation().getName();
    }
}
