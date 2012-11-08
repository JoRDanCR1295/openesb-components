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
 * @(#)HL7V3TransmissionWrapperContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.support.hl7v3;

import java.util.Set;

/**
 * A context class which holds information needed by a sending
 * application or message handling service to package and route the V3 Composite
 * Message to the designated receiving application(s) and/or message handling
 * service(s)
 * 
 * @author S. Nageswara Rao, Raghunadh
 * 
 */
public class HL7V3TransmissionWrapperContext {

	// A unique identifier for this message. The equivalent Version 2 mapping:
	// MSH-10 Message Control ID
	private String id;

	// The time that the message was created. The equivalent Version 2 mapping:
	// MSH-7 Date/Time Of Message
	private String creationTime;

	// defines whether the message is part of a production, training, or
	// debugging system.
	// The equivalent Version 2 mapping: MSH-11 Processing ID component 1
	// (processing id)
	private String processingCode;

	//The identification of the unique information interchange
	private String interactionId;

	// identifies the conditions under which accept acknowledgements are
	// required to be returned in response to this message.
	// Note that accept acknowledgement address two different issues at the same
	// time: reliable transport as well as syntactical correctness.
	// The equivalent Version 2 mapping: MSH-15 Accept Acknowledgment Type
	private String acceptAckCode;

	// The HL7 version used by the sending system; matched by the receiving
	// system to its own version to be sure the message will be interpreted
	// correctly.
	// This version indicates the publication "package" as a whole. The
	// publication package (standard, DSTU, realm subset, etc.)
	// is a collection of many artifacts of which a snapshot is taken and the
	// version is that of all the artifacts in the release as a whole.
	// The equivalent Version 2 mapping: MSH-12 Version ID.
	private String versionCode;

	//The identification of one or more message profiles. 
	//The sender indicates that the message conforms to these message profiles; a receiving system may use this to correctly validate and interpret the message.
	//The equivalent Version 2 mapping: MSH-21 Message Profile Identifier
	private Set profileId;

	//provided for implementing the sequence number protocol (incremented by one for each subsequent value assignment). 
	//The equivalent Version 2 mapping: MSH-13 Sequence Number
	private int sequenceNumber;

	/**
	 * Default constructor.
	 */
	public HL7V3TransmissionWrapperContext() {
	}

	/**
	 * Sets the unique identifier of the HL7 message
	 * @param id
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * Gets the unique identifier of the HL7 message
	 * @param id
	 */
	public String getId() {
		return this.id;
	}

	/**
	 * Sets the message creation time
	 * @param creationTime
	 */
	public void setCreationTime(String creationTime) {
		this.creationTime = creationTime;
	}

	/**
	 * Gets the message creation time
	 * @return
	 */
	public String getCreationTime() {
		return this.creationTime;
	}

	public void setInteractionId(String interactionId) {
		this.interactionId = interactionId;
	}

	public String getInteractionId() {
		return this.interactionId;
	}

	public void setProcessingCode(String processingCode) {
		this.processingCode = processingCode;
	}

	public String getProcessingCode() {
		return this.processingCode;
	}

	public void setAcceptAckCode(String acceptAckCode) {
		this.acceptAckCode = acceptAckCode;
	}

	public String getAcceptAckCode() {
		return this.acceptAckCode;
	}

	public void setVersionCode(String versionCode) {
		this.versionCode = versionCode;
	}

	public String getVersionCode() {
		return this.versionCode;
	}

	public void setSequenceNumber(int seqNumber) {
		this.sequenceNumber = seqNumber;
	}

	public int getSequenceNumber() {
		return this.sequenceNumber;
	}
}
