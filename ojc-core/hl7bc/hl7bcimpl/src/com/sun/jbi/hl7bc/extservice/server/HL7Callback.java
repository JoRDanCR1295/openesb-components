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
 * @(#)HL7Callback.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.server;

/**
 * @author Sriram, S. Nageswara Rao
 * @version 
 */
public interface HL7Callback {

    /**
     * Invoked when there is a reply for the request
     * @param hl7Message - Represents the hl7 ack/nak to be returned to HL7 external sytem 
     */
    void onReply(String hl7Message, boolean isAck) throws com.sun.jbi.hl7bc.ApplicationException, Exception;
  
    /**
     * increases the Nak Sent Count
     */
    void increaseNakSentCount();
    
    /**
     * Gets the Nak Sent Count
     * @return long
     */
    long getNakSentCount();
    
    /**
     * increases the Canned Nak Sent Count
     */
    void increaseCannedNakSentCount();
    
    /**
     * Gets the Canned Nak Sent Count
     * @return long
     */
    long getCannedNakSentCount();

    /**
     * Helps close the connection with the HL7 external sytem
     *
     */
    void closeConnection();
    
    /***
     * Helps in knowing the client details
     */
    String getClientInfo();
}
