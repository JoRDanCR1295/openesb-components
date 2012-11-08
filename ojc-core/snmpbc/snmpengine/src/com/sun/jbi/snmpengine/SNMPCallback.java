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
 * @(#)SNMPCallback.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpengine;

import javax.xml.transform.Source;

/**
 * SNMP callback partially abstracted from JDMK to facilitate a phased implementation 
 * 
 * @author fkieviet
 */
public interface SNMPCallback {
    /**
     * Sends a batch of traps to a particular trap processor. This method should return
     * immediately. Acknowledgments will be returned on the SNMPRA
     * 
     * @param batchid Identifies the batch so that the BC can acknowledge that the 
     * batch was processed
     * @param trapProcessorID identifies to which processor the traps should be sent
     * @param traps traps
     */
    void deliverTraps(String batchid, String trapProcessorID, Source traps);
    
    /**
     * The container implements this to get metadata on network elements; this method
     * should return immediately; the response should be returned through the SNMPRA.
     * 
     * @param queryId identifies the query, so that a response can be correlated
     * @param query the query
     */
    void getMetaData(String queryId, Source query);
    
    
    /**
     * SNMP engine reply to PM request through this method
     *
     * @param msgExchangeId identifier associated with corresponding PM request
     * @param response xml source of response
     */
    void replyPM(String msgExchangeId, Source response);
    
}
