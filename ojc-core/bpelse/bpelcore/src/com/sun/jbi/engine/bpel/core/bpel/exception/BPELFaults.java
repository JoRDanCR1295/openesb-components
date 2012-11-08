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
 * @(#)BPELFaults.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.exception;


/**
 * BPELFaults Class
 *
 * @author Sun Microsystems
 * @version
 *
 * @since 5.0
 */
public class BPELFaults {
    /**
     * The target Namespace prefix for the faults this can be over ridden by constructing a new
     * instance, by default "http:////schemas.xmlsoap.org/ws/2003/03/business-process/"
     */

    //public static String BPWS = "bpws:";

    /**
     * selection failure - Thrown when a selection operation performed either in a function such as
     * bpws:getVariableData or in an assignment, encounters an error
     */
    /** bpws selection failure */
    public static final String SELECTION_FAILURE = "bpws:selectionFailure";

    /** bpws conflicting receive failure */
    public static final String CONFLICTING_RECEIVE = "bpws:conflictingReceive";

    /** bpws conflicting request failure */
    public static final String CONFLICTING_REQUEST = "bpws:conflictingRequest";

    /** bpws mismatched assignment failure */
    public static final String MISMATCHED_ASSIGNMENT_FAILURE = "bpws:mismatchedAssignmentFailure";

    /** bpws join failure */
    public static final String JOIN_FAILURE = "bpws:joinFailure";

    /** bpws forced termination failure */
    public static final String FORCED_TERMINATION = "bpws:forcedTermination";

    /** bpws correlation violation failure */
    public static final String CORRELATION_VIOLATION = "bpws:correlationViolation";

    /** bpws uninitialized variable failure */
    public static final String UNINITIALIZED_VARIABLE = "bpws:uninitializedVariable";

    /** bpws repeated compensation failure */
    public static final String REPEATED_COMPENSATION = "bpws:repeatedCompensation";

    /** bpws invalid reply failure */
    public static final String INVALID_REPLY = "bpws:invalidReply";

    public static final String PROCESSING_ERROR = "ProcessingError";

    /*
       public BPELFaults (String bpwsPrefix) {
           if (bpwsPrefix != null && bpwsPrefix.trim().equals("")){
               this.BPWS = bpwsPrefix;
           } else {
               //System.out.println (" the bpwsPrefix is not overridden " );
           }
       }
     */
}
