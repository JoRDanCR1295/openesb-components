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
 * @(#)ToDoListener.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.common.todotask;

import java.util.EventListener;

/**
 * Describes a listener of to-do events.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface ToDoListener extends EventListener {
    
    /** Respond to to-do events.
     * @param   toDoEvent   To-do event.
     * @return  <code>true</code> if more events can be accepted by the listener;
     *          <code>false</code> otherwise.
     */
    boolean toDoPerformed(ToDoEvent toDoEvent);
    
}
