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
 * @(#)EventHandlersChildAct.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime;

/**
 * An interface to represent onEvent or onAlarm unit
 * within EventHandlers
 *
 * @author Sun Microsystems
 *
 */
public interface EventHandlersChildAct extends ActivityUnit {
    
    /**
     * Returns the parent RuntimeEventHandlers
     * @return
     */    
    RuntimeEventHandlers getRuntimeEventHandlers ();
    
    /**
     * Returns the unique EventHandler Child GUID that is 
     * generated when the Child activities of the EventHandlers
     * are created 
     * @return String - value of the GUID.
     */
    String getUID();

}
