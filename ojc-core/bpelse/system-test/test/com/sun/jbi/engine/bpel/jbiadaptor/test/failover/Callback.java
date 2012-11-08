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
 * @(#)Callback.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.failover;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;

/**
 * Callback interface that can be used by the junits to intercept 
 * the invokes to engine channel
 * 
 * @author mbhasin
 *
 */
public interface Callback {

    /**
     * One way invoke callback.
     * 
     * @param engineSimulator
     * @param messageContainer
     * @param process
     * @param msgExId
     * @return
     */
    public boolean oneWayInvokeCallback(EngineSimulator engineSimulator,
                                        MessageContainer messageContainer,
                                        RBPELProcess process,
                                        String msgExId);

    /**
     * Two way invoke callback.
     * 
     * @param engineSimulator
     * @param msgCont
     * @param process
     * @param msgExId
     * @return
     */
    public boolean twoWayInvokeCallback(EngineSimulator engineSimulator,
                                        MessageContainer msgCont,
                                        RBPELProcess process,
                                        String msgExId);

    /**
     * To provide callback on two way invoke call to sub business process.
     * 
     * @param engineSimulator
     * @param msgCont
     * @param model
     * @param msgExId
     * @return
     */
    public boolean twoWaySubBPInvokeCallback(EngineSimulator engineSimulator,
                                             MessageContainer msgCont,
                                             InComingEventModel model,
                                             String msgExId);
    
    /**
     * This interface would notify regarding the status message for 
     * in-only request sent to engine. The only action that is performed
     * by the engine channel simulator on this callback is to call commit
     * or rollback on the open transaction. This callback method provides 
     * hook to perform custom functions.
     * 
     * @param engineSimulator
     * @param msgExchangeId
     */
    public void sendInOnlyRequestStatusCallback(EngineSimulator engineSimulator,
    										String msgExchangeId);

    /**
     * Reply callback.
     * 
     * @param engineSimulator
     * @param messageContainer
     * @param model
     */
    public void replyCallback(EngineSimulator engineSimulator,
                              MessageContainer messageContainer,
                              InComingEventModel model);
}
