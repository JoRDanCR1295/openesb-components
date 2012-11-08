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
 * @(#)ScopeOrProcessUnit.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime;


/**
 * A runtime interface to encapsulate the common operations available
 * in Scope and Process
 * 
 * @author Sun MicroSystems
 *
 */
public interface ScopeOrProcessUnit {

    /**
     * Instantiate all registered EventHandlers.
     *
     */
    void instantiateEventHandlers() throws Exception;

    
    /**
     *  Will be called by the event handlers execution model, once the event 
     *  handler is complete and it has been notified that the scope is also 
     *  complete. This is a callback to the unit from the event handlers.
     *  
     *  @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers#associatedInstanceComplets()
     */
    void eventHandlersComplete();
}
