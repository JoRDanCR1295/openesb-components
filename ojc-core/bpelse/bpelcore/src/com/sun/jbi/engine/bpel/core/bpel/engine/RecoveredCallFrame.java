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
 * @(#)RecoveredCallFrame.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine;



/**
 * recovered callframe
 *
 * @author Sun Microsystems
 */
public interface RecoveredCallFrame extends ICallFrame {
    /**
     * persisted time value which will help the wait and onalarm  activities to determine how much
     * time to wait for after recovery.
     *
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame#getTimerVal()
     */
    long getTimerVal();

    /**
     * convert
     *
     * @return ICallFrame callframe
     */
    ICallFrame convert();
    
    /** Flag to determine if this callframe is because of flow that has a corresponding entry in 
     * the last check point table or not.   
     * @return
     */
    boolean hasPersistedChildCF();
    
    /**sets the flag to determine if this callframe is because of flow that has a corresponding entry in 
     * the last check point table or not. 
     * @param reconstructedFlow
     */
    void setHasPersistedChildCF(boolean reconstructedFlow);
}
