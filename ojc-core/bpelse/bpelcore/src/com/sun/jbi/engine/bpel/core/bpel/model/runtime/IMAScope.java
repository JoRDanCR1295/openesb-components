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
 * @(#)IMAScope.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime;

import com.sun.bpel.model.meta.RReply;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELRuntimeException;

/**
 * The IMAScope provides the interface which an Inbound Message Activity (IMA)
 * uses to save its incoming requests, and matched by the corresponding
 * reply in the same scope
 * 
 */
public interface IMAScope {
    /**
     * Process an IMA, retains the req msg so that the reply in the same context can
     * find the corresponding receive
     *
     * @param rcv activity
     * @param req event
     *
     * @throws BPELRuntimeException on error
     */
    void addRequest(RStartElement rcv, MessageContainer req)
    throws BPELRuntimeException;
    
    /**
     * remove the request associated to receive.
     *
     * @param rcv activity
     * @param req event
     *
     * @throws BPELRuntimeException on error
     */
    void removeRequest(RStartElement rcv, MessageContainer req)
    throws BPELRuntimeException;

    /**
     * Adding the list of entries in the CRMP table that do not have a response object set yet
     * This is required to check if persistence at the end of the reply activity need 
     * to include an update statment for CRMP table
     * The value insterted is a string concatenation of "bpid + partnerlink + operation" 
     * @param updateValueKey
     */
    void addToCRMPUpdateList(String updateValueKey);

    /**
     * Check to see if the there is an CRMP entry in this list to update for the reply activity 
     * persistence.
     * @param updateValueKey
     * @return boolean value 
     */
    boolean crmpUpdateListContains(String updateValueKey);

    /**
     * Get the request associated with a reply activity
     *
     * @param reply The reply activity
     *
     * @return The request
     */
    MessageContainer removeRequest(RReply reply);     

    /**
     * Set the fault on the pending requests
     * @param error Exception object representing the error 
     * Addition: When the 'atomicTxType' is set to 'Required' and the bpel-se starts a Tx when the inbound message 
     * is not associated with one, then in this method, we will rollback the started Tx, since this instance has not
     * completed in an orderly fashion(it is in error or has faulted).
     */
    void sendErrorsForPendingRequests(Exception error); 

    /**
     * Declares a default messageExchange.
     * From section 10.4.1
     * If the messageExchange attribute is not specified on an IMA or <reply> then 
     * the activity's messageExchange is automatically associated with a default 
     * messageExchange with no name. Default messageExchange's are implicitly 
     * declared by the <process> and the immediate child scopes of <onEvent> 
     * and the parallel form of <forEach>.
     * @return true 
     */
    void declareDefaultMessageExchange();
    
    /**
     * Sends done status on pending InOnly requests. When the BP is marked atomic, the done status is sent for 
     * the request only at the completion of the process (or scope if the message exchange is active only within that
     * scope)
     * Addition: When the 'atomicTxType' is set to 'Required' and the bpel-se starts a Tx when the inbound message 
     * is not associated with one, then in this method, we will commit the started Tx, since this instance has completed
     * in an orderly fashion. 
     */
    void completePendingInOnlyRequests();

}
