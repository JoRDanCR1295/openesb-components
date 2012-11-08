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
 * @(#)BPELProcessOrScopeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.BPELProcessOrScope;
import com.sun.bpel.model.EventHandlers;

/**
 *
 * @author Sun Microsystems
 */
public abstract class BPELProcessOrScopeImpl extends BPELElementImpl implements BPELProcessOrScope {
    
    /** Holds value of property eventHandlers. */
    private EventHandlers eventHandlers;
    
    /** Creates a new instance of BPELProcessOrScopeImpl */
    public BPELProcessOrScopeImpl() {
    }
    
    
    /** Getter for property eventHandlers.
     * @return Value of property eventHandlers.
     *
     */
    public EventHandlers getEventHandlers() {
        return eventHandlers;
    }
    
    /** Setter for property eventHandlers.
     * @param eventHandlers New value of property eventHandlers.
     *
     */
    public void setEventHandlers(EventHandlers eventHandlers) {
        EventHandlers oldEventHandlers = this.eventHandlers;
    	this.eventHandlers = eventHandlers;
        super.replaceChild(7, oldEventHandlers, eventHandlers);
    }
}
