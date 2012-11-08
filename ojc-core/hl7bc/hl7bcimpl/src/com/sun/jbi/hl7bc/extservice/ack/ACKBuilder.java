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
 * @(#)ACKBuilder.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.ack;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import com.sun.jbi.hl7bc.HL7Constants;

/**
 * The implementers of this interface should build the ACK conforming to the exact HL7 version 
 * 
 * @author S. Nageswara Rao
 *
 */

public interface ACKBuilder extends HL7Constants {
	
	//MSH segment of the HL7 Request
	void setMSHSegment(Node node);
	//Acknowledgment code which would be used to set MSA-1-acknowledgment code 
	void setAcknowledgmentCode(String ackCode);
	void setErrorCode(String errCode);
	//Error Message
	void setErrorMessage(String errMsg);
    //Expected Sequnece Number
    void setExpectdSeqNo(int expSeqNo);
	//returns the Dom Node representing the acknowledgment
	Document buildACK() throws Exception;
}
