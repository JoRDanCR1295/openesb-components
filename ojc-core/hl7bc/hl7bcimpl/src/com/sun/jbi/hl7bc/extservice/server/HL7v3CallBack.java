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
 * @(#)HL7v3CallBack.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.server;

/**
 * This interface is used by to acknowledge HL7 v3.0 message sending system
 * 
 * MLLP v2.0 specification prescribed that Message Receiver needs to send
 * Affirmative Commit Acknowledgement when HL7 content has been successfully
 * committed to the persistent storage. Otherwise send a Negative Commit
 * Acknowledgement. It is presumed that BPEL SE would persist the message.
 * 
 * 
 */
public interface HL7v3CallBack extends HL7Callback {

	/**
	 * This method will be used to trigger either ACK or NAK to HL7 v3 message sending
	 * system.
	 * 
	 * @param persistFailure -
	 *            indicates whether persisting the message is successful or not
	 */
	void sendMLLPv2TransportLayerACK(boolean persistFailure);

}
