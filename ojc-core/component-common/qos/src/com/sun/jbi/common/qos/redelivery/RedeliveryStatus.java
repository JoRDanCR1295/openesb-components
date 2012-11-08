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
 * @(#)RedeliveryStatus.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.redelivery;

/**
 * Defines contract for the redelivery status of a message exchange. 
 * @author Kevan Simpson
 */
public interface RedeliveryStatus {
    /**
     * Fetches a timestamp of the last redelivery attempt.
     * @return a timestamp of the last redelivery attempt.
     */
    long getLastRetryTime();
    
    /**
     * Fetches the maximum number of redelivery attempts.
     * @return the maximum number of redelivery attempts.
     */
    int getTotalRetries();
    
    /**
     * Fetches the remaining number of redelivery attempts.
     * @return the remaining number of redelivery attempts.
     */
    int getRemainingRetries();
    
    /**
     * Fetches the error causing last redelivery attempt to fail.
     * @return the error causing last redelivery attempt to fail.
     */
    Exception getError();

    /**
     * Returns <code>true</code> if redelivery attempts have been exhausted and
     * further retry attempts are not permitted.
     * @return <code>true</code> if redelivery attempts have been exhausted.
     */
    boolean hasFailed();
}
