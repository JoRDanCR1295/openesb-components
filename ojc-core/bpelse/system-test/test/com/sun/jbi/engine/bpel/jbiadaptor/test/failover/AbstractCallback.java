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
 * @(#)AbstractCallback.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.failover;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;

public class AbstractCallback implements Callback {

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.jbiadaptor.test.failover.Callback#oneWayInvokeCallback(com.sun.jbi.engine.bpel.jbiadaptor.test.failover.EngineSimulator, com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer, com.sun.bpel.model.meta.RBPELProcess, java.lang.String)
     */
    public boolean oneWayInvokeCallback(EngineSimulator engineSimulator, MessageContainer messageContainer, RBPELProcess process, String msgExId) {
        return true;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.jbiadaptor.test.failover.Callback#twoWayInvokeCallback(com.sun.jbi.engine.bpel.jbiadaptor.test.failover.EngineSimulator, com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer, com.sun.bpel.model.meta.RBPELProcess, java.lang.String)
     */
    public boolean twoWayInvokeCallback(EngineSimulator engineSimulator, MessageContainer msgCont, RBPELProcess process, String msgExId) {
        return true;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.jbiadaptor.test.failover.Callback#twoWaySubBPInvokeCallback(com.sun.jbi.engine.bpel.jbiadaptor.test.failover.EngineSimulator, com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer, com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel, java.lang.String)
     */
    public boolean twoWaySubBPInvokeCallback(EngineSimulator engineSimulator, MessageContainer msgCont, InComingEventModel model, String msgExId) {
        return true;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.jbiadaptor.test.failover.Callback#sendInOnlyRequestStatusCallback(com.sun.jbi.engine.bpel.jbiadaptor.test.failover.EngineSimulator, java.lang.String)
     */
    public void sendInOnlyRequestStatusCallback(EngineSimulator engineSimulator, String msgExchangeId) {
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.jbiadaptor.test.failover.Callback#replyCallback(com.sun.jbi.engine.bpel.jbiadaptor.test.failover.EngineSimulator, com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer, com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel)
     */
    public void replyCallback(EngineSimulator engineSimulator, MessageContainer messageContainer, InComingEventModel model) {
    }
}
