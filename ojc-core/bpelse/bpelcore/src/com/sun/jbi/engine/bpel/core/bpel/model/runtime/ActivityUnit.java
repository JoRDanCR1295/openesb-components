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
 * @(#)ActivityUnit.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime;

import com.sun.bpel.model.meta.RActivity;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame;


/**
 * Activity unit interface
 *
 * @author Sun Microsystems
 */
public interface ActivityUnit extends Unit {
    /**
     * gets static model activity
     *
     * @return RActivity static model activity
     */
    RActivity getStaticModelActivity();

    /**
     * gets enclosing activity unit
     *
     * @return ActivityUnit enclosing activity unit
     */
    ActivityUnit getEnclosingActivityUnit();

    /**
     * gets next activity unit
     *
     * @return ActivityUnit next activity unit
     */
    ActivityUnit getNextActivityUnit();

    /**
     * gets previous activity unit
     *
     * @return ActivityUnit previous activity unit
     */
    ActivityUnit getPrevActivityUnit();

    /**
     * Persistence related functionality.
     *
     * @return long branch ID
     */
    long getBranchId();

    /**
     * executes recovery
     *
     * @param frame recovered callframe
     * @param bpit BP process instance thread
     * @param rObjs required object
     *
     * @return boolean: on successful completion of the action, returns true; otherwise, returns
     *         false
     *
     * @throws Exception Exception
     */
    boolean doActionOnRecovery(
        RecoveredCallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception;
}
