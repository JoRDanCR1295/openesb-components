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
 * @(#)MessageExchangeReplyListener.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.InOut;

/**
 * Listener interface for processing reply.
 * 
 * @author S. Nageswara Rao
 */
public interface MessageExchangeReplyListener {
    /**
     * Process reply
     * 
     * @param me MessageExchange to process.
     * @param processReplySuccess Set to true so that the listener can complete processing the
     *            inbound HL7 message delivery as a success; process the inbound HL7 message
     *            delivery as error if set to false.
     */
    void onReply(MessageExchange me, boolean processReplySuccess) throws ApplicationException;
    
    
    /**
     * Process the "output" of an InOut message exchange
     *
     * @param inout The InOut message exchange created by the
     *        inbound HL7 BC to process the HL7 message request/response.
     *
     * @throws Exception upon error processing the output.
     */          
    public void onOutput (InOut inout) throws ApplicationException, Exception;
}
