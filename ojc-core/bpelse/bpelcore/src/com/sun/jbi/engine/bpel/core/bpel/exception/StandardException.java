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
 * @(#)CopyUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.exception;

/**
 * 
 * @author Sun Microsystems
 *
 */
public class StandardException extends BPELException {

	public enum Fault {
		AmbiguousReceive("ambiguousReceive") ,
		CompletionConditionFailure("completionConditionFailure"),
		ConflictingReceive("conflictingReceive"),
		ConflictingRequest("conflictingRequest"),
		CorrelationViolation("correlationViolation"),
		InvalidBranchCondition("invalidBranchCondition"),
		InvalidExpressionValue("invalidExpressionValue"),
		InvalidVariables("invalidVariables"),
		JoinFailure("joinFailure"),
		MismatchedAssignmentFailure("mismatchedAssignmentFailure"),
		MissingReply("missingReply"),
		MissingRequest("missingRequest"),
		ScopeInitializationFailure("scopeInitializationFailure"),
		SelectionFailure("selectionFailure"),
		SubLanguageExecutionFault("subLanguageExecutionFault"),
		UninitializedPartnerRole("uninitializedPartnerRole"),
		UninitializedVariable("uninitializedVariable"),
		UnsupportedReference("unsupportedReference"),
		XsltInvalidSource("xsltInvalidSource"),
		XsltStylesheetNotFound("xsltStylesheetNotFound");
		
		private String mStrVal;
		
		private Fault(String value) {
			mStrVal = value;
		}
		
		public String toString() {
			return mStrVal;
		}
	}
	
	/* */
	private Fault mFault;
	
	/**
	 * 
	 * @param stdFault
	 */
	public StandardException(Fault fault) {
		super();
		mFault = fault;
	}

	/**
	 * 
	 * @param stdFault
	 * @param message
	 * @param cause
	 */
	public StandardException(Fault fault, String message, Throwable cause) {
		super(message, cause);
		mFault = fault;
	}

	/**
	 * 
	 * @param stdFault
	 * @param message
	 */
	public StandardException(Fault fault, String message) {
		super(message);
		mFault = fault;
	}

	/**
	 * 
	 * @param stdFault
	 * @param cause
	 */
	public StandardException(Fault fault, Throwable cause) {
		super(cause);
		mFault = fault;
	}

	/**
	 * 
	 * @return
	 */
	public Fault getFault() {
		return mFault;
	}

}
