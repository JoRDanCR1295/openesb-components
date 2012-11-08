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
 * @(#)Unit.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime;

import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;


/**
 * Unit interface
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Unit {
    /**
     * execute action
     *
     * @param frame callframe
     * @param bpit BP process insntance thread
     * @param rObjs required objects
     *
     * @return true if the action is complete, false if the action is wating for an event.
     *         (incoming message, timer, etc).
     *
     * @throws Exception Exception
     */
    boolean doAction(ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs)
        throws Exception;

    /**
     * gets enclosing scope activity unit
     *
     * @return ActiivityUnit enclosing scope activity unit
     */
    ActivityUnit getEnclosingScopeUnit();

    /**
     * gets enclosing unit
     *
     * @return Unit enclosing unit
     */
    Unit getEnclosingUnit();
    
    /** This API is required for recovery and will return the context this Unit is
     * associated with.
     * For activities that are context by itself(ex: scope, invoke) this will return
     * the activity itself, for others the parent context is returned.
     * Note: Use this api for persisitence/recovery related calls.
     * @return Context object (immediate context) that this unit is associated with.
     */
    Context getContext();
    
    
}
