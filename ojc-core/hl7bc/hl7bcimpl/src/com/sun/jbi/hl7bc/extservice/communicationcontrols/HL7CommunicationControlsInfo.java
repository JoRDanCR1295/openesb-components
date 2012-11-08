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
 * @(#)HL7CommunicationControlsInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.communicationcontrols;

import java.util.Map;

import com.sun.jbi.hl7bc.Endpoint;
import com.sun.jbi.hl7bc.HL7Constants;
import com.sun.jbi.hl7bc.extensions.HL7CommunicationControls;
import com.sun.jbi.hl7bc.extensions.HL7CommunicationControl;

public class HL7CommunicationControlsInfo implements HL7Constants {

    private HL7CommunicationControls mHL7CommunicationControls;

    private HL7CommunicationControl mHL7CommunicationControl;

    private Map<String, HL7CommunicationControl> hl7CommControlEntries;

    public HL7CommunicationControlsInfo(Endpoint endpoint) {
        mHL7CommunicationControls = endpoint.getHL7CommunicationControls();
        if (mHL7CommunicationControls != null)
            hl7CommControlEntries = mHL7CommunicationControls.getCommunicationControls();
    }

    /**
     * get the HL7CommunicationControl for TIME_TO_WAIT_FOR_RESPONSE
     * 
     * @return mHL7CommunicationControl HL7CommunicationControl
     */
    public HL7CommunicationControl getTimeToWaitCommControl() {
        if (hl7CommControlEntries == null || hl7CommControlEntries.size() <= 0
                || !hl7CommControlEntries.containsKey(TIME_TO_WAIT_FOR_RESPONSE)) {
            return null;
        }
        mHL7CommunicationControl = hl7CommControlEntries.get(TIME_TO_WAIT_FOR_RESPONSE);
        return mHL7CommunicationControl;
    }

    /**
     * get the HL7CommunicationControl for MAX_NO_RESPONSE
     * 
     * @return mHL7CommunicationControl HL7CommunicationControl
     */
    public HL7CommunicationControl getMaxNoResponseCommControl() {
        if (hl7CommControlEntries == null || hl7CommControlEntries.size() <= 0
                || !hl7CommControlEntries.containsKey(MAX_NO_RESPONSE)) {
            return null;
        }
        mHL7CommunicationControl = hl7CommControlEntries.get(MAX_NO_RESPONSE);
        return mHL7CommunicationControl;
    }

    /**
     * get the HL7CommunicationControl for MAX_NAK_RECEIVED
     * 
     * @return mHL7CommunicationControl HL7CommunicationControl
     */
    public HL7CommunicationControl getMaxNakReceivedCommControl() {
        if (hl7CommControlEntries == null || hl7CommControlEntries.size() <= 0
                || !hl7CommControlEntries.containsKey(MAX_NAK_RECEIVED)) {
            return null;
        }
        mHL7CommunicationControl = hl7CommControlEntries.get(MAX_NAK_RECEIVED);
        return mHL7CommunicationControl;
    }
    
    /**
     * get the HL7CommunicationControl for MAX_NAK_SENT
     * 
     * @return mHL7CommunicationControl HL7CommunicationControl
     */
    public HL7CommunicationControl getMaxNakSentCommControl() {
        if (hl7CommControlEntries == null || hl7CommControlEntries.size() <= 0
                || !hl7CommControlEntries.containsKey(MAX_NAK_SENT)) {
            return null;
        }
        mHL7CommunicationControl = hl7CommControlEntries.get(MAX_NAK_SENT);
        return mHL7CommunicationControl;
    }
    
    /**
     * get the HL7CommunicationControl for MAX_CANNED_NAK_SENT
     * 
     * @return mHL7CommunicationControl HL7CommunicationControl
     */
    public HL7CommunicationControl getMaxCannedNakSentCommControl() {
        if (hl7CommControlEntries == null || hl7CommControlEntries.size() <= 0
                || !hl7CommControlEntries.containsKey(MAX_CANNED_NAK_SENT)) {
            return null;
        }
        mHL7CommunicationControl = hl7CommControlEntries.get(MAX_CANNED_NAK_SENT);
        return mHL7CommunicationControl;
    }
    
    
    /**
     * get the HL7CommunicationControl for NAK_RECEIVED
     * 
     * @return mHL7CommunicationControl HL7CommunicationControl
     */
    public HL7CommunicationControl getNakReceivedCommControl() {
        if (hl7CommControlEntries == null || hl7CommControlEntries.size() <= 0
                || !hl7CommControlEntries.containsKey(NAK_RECEIVED)) {
            return null;
        }
        mHL7CommunicationControl = hl7CommControlEntries.get(NAK_RECEIVED);
        return mHL7CommunicationControl;
    }

    /**
     * get the HL7CommunicationControl for MAX_EMPTY_READ_RETRY
     * 
     * @return mHL7CommunicationControl HL7CommunicationControl
     */
    public HL7CommunicationControl getMaxeEmptyReadCommControl() {
        if (hl7CommControlEntries == null || hl7CommControlEntries.size() <= 0
                || !hl7CommControlEntries.containsKey(MAX_EMPTY_READ_RETRY)) {
            return null;
        }
        mHL7CommunicationControl = hl7CommControlEntries.get(MAX_EMPTY_READ_RETRY);
        return mHL7CommunicationControl;
    }

    /**
     * get the HL7CommunicationControl for MAX_CONNECTION_RETRIES
     * 
     * @return mHL7CommunicationControl HL7CommunicationControl
     */
    public HL7CommunicationControl getMaxConnectionRetriesControl() {
        if (hl7CommControlEntries == null || hl7CommControlEntries.size() <= 0
                || !hl7CommControlEntries.containsKey(MAX_CONNECTION_RETRIES)) {
            return null;
        }
        mHL7CommunicationControl = hl7CommControlEntries.get(MAX_CONNECTION_RETRIES);
        return mHL7CommunicationControl;
    }
}
