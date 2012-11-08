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
 * @(#)PollingPattern.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc.extensions;

/**
 * Enumeration type for polling pattern.
 * 
 * @author Jun Xu
 * @version $Revision: 1.1 $
 */
public enum PollingPattern {
    
    /**
     * Repetitively invoke a command based on certain interval and for every
     * invoking send the result to NMR as a normalized message.
     */
    REPETITIVE_INVOKE_AND_RECEIVE,
    
    /**
     * Invoke the command once and keep receiving the result via stdout.
     * Assuming the result is delimited into records and each record will
     * be sent as a normalized message.
     */
    INVOKE_ONCE_AND_KEEP_RECEIVING
}
