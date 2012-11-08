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
 * @(#)RedeliveryConfig.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.redelivery;

import com.sun.jbi.common.qos.ServiceQuality;

/**
 * Defines contract for redelivery configuration.
 * @author Kevan Simpson
 */
public interface RedeliveryConfig extends ServiceQuality {
    /** Enum to describe available on-failure actions. */
	public enum Failure { redirect, suspend, delete, error };

    /**
     * Fetches the maximum number of redelivery attempts.
     * @return the maximum number of redelivery attempts.
     */
    public int getMaxRetries();
    
    /**
     * Fetches the interval between redelivery attempts.
     * @return the interval between redelivery attempts.
     */
    public long getRetryInterval();
    
    /**
     * Fetches the endpoint, if configured, to which a message
     * is sent when its redelivery attempts are exhausted.
     * @return a redirect endpoint or <code>null</code>.
     */
    public Redirect getRedirect();
    
    /**
     * Fetches the failure action to apply when redelivery attempts are exhausted.
     * @return the failure action to apply when redelivery attempts are exhausted.
     */
    public Failure getFailure();
}
