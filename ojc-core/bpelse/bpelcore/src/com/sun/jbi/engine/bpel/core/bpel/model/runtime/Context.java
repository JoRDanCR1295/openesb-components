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
 * @(#)Context.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime;


import java.util.Collection;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;

/**
 * The Context represents a 'context' in which a unit executes. The context provides acesses to
 * variables, associates correlations and provides methods  to delegate fault handling. This
 * interface is to be implemented by  BPELProcessInstanceImpl (ProcessUnit), ScopeUnit, CatchUnit
 * and InvokeUnit.  When a Unit is created it must be passed a Context. If the Unit being created
 * is a Context itself, it will set the passed context as its parent
 *
 * @author pbhagat
 */
public interface Context extends VariableScope, IMAScope, PartnerLinkScope {
    
	/**
     * @return BPELProcessInstance that the context is associated with. It is a n:1
     * relationship with BPELProcessInstance. There will be many contexts associated 
     * with one BPELProcessInstance
     */
    BPELProcessInstance getProcessInstance();
    
    /** API to update the running state of the instance after recovery. This API is 
     * expected to be called only during recovery
     * @param runtimeVariables
     * @param runtimePLinks TODO
     */
    void initUponRecovery(Collection<RuntimeVariable> runtimeVariables, Collection<RuntimePartnerLink> runtimePLinks);
    
    /**
     * @param rEH
     */
    void initEHUponRecovery(RuntimeEventHandlers rEH);
    
    /** 
     * @return TODO
     */
    Context getParentContext();
    
    /**
     * Gets a reference to the FaultHandlingContext associated with the Unit
     * @return
     */
    FaultHandlingContext getFaultHandlingContext();
    
    StateContext getStateContext();
}

interface CorrelationsScope {
    //For future use. Appropriate methods should be idenfied and added here.
    //If needed should be moved to a separate file
}
