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
 * @(#)BPELProcessOrScope.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import java.util.Collection;

/**
 * 
 * @author Sun Microsystems
 *
 */
public interface BPELProcessOrScope extends VariableScope,
                                            BPELElement, 
                                            EventHandlersHolder,
                                            CompensationHandlerHolder, FaultHandlerScope {

	/** Getter for property partnerLinks.
     * @return Value of property partnerLinks.
     *
     */
    PartnerLinks getPartnerLinks();
    
    /** Setter for property partnerLinks.
     * @param partners New value of property partnerLinks.
     *
     */
    void setPartnerLinks(PartnerLinks partnerLinks);
    
	 /** Getter for property variables.
     * @return  Value of property variables.
     */
    Variables getVariables();
   
    /** Setter for property variables.
     * @param   variables  New value of property variables.
     */
    void setVariables(Variables variables);
    
    /** Getter for property correlationSets.
     * @return Value of property correlationSets.
     *
     */
    CorrelationSets getCorrelationSets();
    
    /** Setter for property correlationSets.
     * @param correlationSets New value of property correlationSets.
     *
     */
    void setCorrelationSets(CorrelationSets correlationSets);
    
    
    /** Setter for property faultHandlers.
     * @param faultHandlers New value of property faultHandlers.
     *
     */
    void setFaultHandlers(FaultHandlers faultHandlers);
    
    /**
     * Get PartnerLink Given its name.
     * @param pName the name of the partnerLink
     * @return the partnerLink or null if not found
     */
    PartnerLink getBPELPartnerLink(String pName);
     
//Move to ScopeActivity
//    /**
//     * Get Variable Given its name.
//     * @param vName the name of the variable
//     * @return the variable or null if not found
//     */
//    Variable getBPELVariable(String vName);
    
    /**
     * Get a collection of all bpel receive activities
     * used in this process. This will find out all the
     * nested receives as well.
     * @return collection of bpel receive activities.
     */
    Collection getAllBPELReceive();
    
    /**
     * Get a collection of all bpel onMessage activities
     * used in this process. This will find out all the
     * nested onMessage as well.
     * @return
     */
    Collection getAllBPELOnMessage();
    
    /**
     * Get a collection of all bpel receive activities
     * used in this process which have given partnerLink. 
     * This will find out all the
     * nested receives as well.
     * @return Collection of bpel receive activities.
     */
    Collection getAllBPELReceive(String partnerLinkName);
    
    /**
     * Get a collection of all bpel onMessage activities
     * used in this process which have given partnerLinkName. 
     * This will find out all the
     * nested onMessage as well.
     * @return Collection of bpel onMessage activities.
     */
    Collection getAllBPELOnMessage(String partnerLinkName);
    
    /**
     * @param name
     * @return
     */
    CorrelationSet getCorrelationSet(String name);
}
