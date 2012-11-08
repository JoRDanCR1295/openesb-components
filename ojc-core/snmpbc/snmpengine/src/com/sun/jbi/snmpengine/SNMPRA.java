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
 * @(#)SNMPRA.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpengine;

import javax.xml.transform.Source;

/**
 * Abstracts the engine, providing a bean interface for configuration
 * 
 * @author fkieviet
 */
public abstract class SNMPRA extends SNMPRAConfig {
    /**
     * Starts the engine on the specified port
     * 
     * @param listener listener
     * @throws Exception on failure
     */
    public abstract void start(SNMPCallback listener) throws Exception;
            
    /**
     * Stops the engine; will NEVER throw an exception; may be called multiple times
     */
    public abstract void stop();
    
    /**
     * Called with an asynchronous response to metadata queries
     * 
     * @param queryId identifies the query
     * @param results results
     */
    public abstract void replyMetadata(String queryId, /*boolean error,*/ Source results);
    
    /**
     * Called when a batch of traps has been delivered processed (used for throttling)
     * 
     * @param batchId identifies the batch
     * @param error an error occurred (ME reported FAULT)
     */
    public abstract void replyTrap(String batchId, boolean error);
    
    /**
     * Called when Performance Manager wants to invoke Get related operations on Network Elements.
     * This method should return immediately.
     * 
     * @param requestId unique identifier for this request
     * @param request request xml source
     * @returns response xml source
     */
    public abstract void requestPM(String requestId, Source request);
}
