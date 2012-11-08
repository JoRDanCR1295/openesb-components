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
 * @(#)DummyActivityUnit.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import com.sun.bpel.model.meta.RActivity;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;


/**
 * Dummy activity unit implementation
 *
 * @author Sun Microsystems
 */
final class DummyActivityUnit implements ActivityUnit {
	
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.Unit#getEnclosingUnit()
     */
    public Unit getEnclosingUnit() {
        throw new RuntimeException(I18n.loc("BPCOR-3024: Invalid method call {0}", getClass().getName()));
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#getStaticModelActivity()
     */
    public RActivity getStaticModelActivity() {
        throw new RuntimeException(I18n.loc("BPCOR-3024: Invalid method call {0}", getClass().getName()));
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#getEnclosingScopeUnit()
     */
    public ActivityUnit getEnclosingScopeUnit() {
        throw new RuntimeException(I18n.loc("BPCOR-3024: Invalid method call {0}", getClass().getName()));
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#getEnclosingActivityUnit()
     */
    public ActivityUnit getEnclosingActivityUnit() {
        throw new RuntimeException(I18n.loc("BPCOR-3024: Invalid method call {0}", getClass().getName()));
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#getNextActivityUnit()
     */
    public ActivityUnit getNextActivityUnit() {
        throw new RuntimeException(I18n.loc("BPCOR-3024: Invalid method call {0}", getClass().getName()));
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#getPrevActivityUnit()
     */
    public ActivityUnit getPrevActivityUnit() {
        throw new RuntimeException(I18n.loc("BPCOR-3024: Invalid method call {0}", getClass().getName()));
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#doAction(ICallFrame,
     *      BusinessProcessInstanceThread, RequiredObjects)
     */
    public boolean doAction(
        ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception {
        throw new RuntimeException(I18n.loc("BPCOR-3024: Invalid method call {0}", getClass().getName()));
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#getBranchId()
     */
    public long getBranchId() {
        throw new RuntimeException(I18n.loc("BPCOR-3024: Invalid method call {0}", getClass().getName()));
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#doActionOnRecovery(RecoveredCallFrame,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread,
     *      com.sun.jbi.engine.bpel.core.bpel.model.RequiredObjects)
     */
    public boolean doActionOnRecovery(
        RecoveredCallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception {
        throw new RuntimeException(I18n.loc("BPCOR-3024: Invalid method call {0}", getClass().getName()));
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit#getContext()
     */
    public Context getContext() {
        // TODO Auto-generated method stub
        return null;
    }
}
