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
 * @(#)StateFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist;

import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.persist.State.Mode;
import com.sun.jbi.engine.bpel.core.bpel.persist.impl.StateImpl;
import com.sun.jbi.engine.bpel.core.bpel.persist.impl.SuspendResumeStateImpl;
import com.sun.jbi.engine.bpel.core.bpel.persist.impl.VariableChangeStateImpl;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class StateFactory {
    private static StateFactory mStateFactory;

    /**
     * gets StateFactory
     *
     * @return StateFactory state factory
     */
    public static StateFactory getStateFactory() {
        if (mStateFactory == null) {
            mStateFactory = new StateFactory();
        }

        return mStateFactory;
    }

    // TODO  caching of state objects can be done, in this factory. when the 
    // state is in the delete-mode,, it could be reused.

    /**
     * create state if the persistence is enabled. returns null if the persistence 
     * is not enabled. 
     *
     * @param eng engine ID
     * @param bpelId BPEL ID
     * @param instance BPID
     *
     * @return MutableState MutableState
     */
    public MutableState createState(Engine eng, QName bpelId, String id) {
    	BPELProcessManager manager = eng.getBPELProcessManager(bpelId);
        if (manager.isPersistenceEnabled()) {
            return new StateImpl(eng.getId(), bpelId, id);
        }
        return null;
    }
    
    /**
     * Create VariableChangeState
     * 
     * @param engId
     * @param bpelId
     * @param bpId
     * @param rvar
     * @return
     */
    public State createVariableChangeState (String engId, QName bpelId, String bpId, RuntimeVariable rvar) {
        return new VariableChangeStateImpl(engId, bpelId, bpId, rvar);
    }
    
    
    public State createSuspendResumeState(String engId, QName bpelId, String bpId, Mode suspendOrResume) {
    	return new SuspendResumeStateImpl(engId, bpelId, bpId, suspendOrResume);
    }
}

