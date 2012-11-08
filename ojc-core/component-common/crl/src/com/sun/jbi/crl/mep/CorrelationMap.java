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
 * @(#)CorrelationMap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep;

/**
 * Correlates important component data with a message exchange by its id.
 * 
 * @author Kevan Simpson
 */
public interface CorrelationMap {
    /**
     * Correlates component data with a message exchange by its id.
     * 
     * @param exchangeId The id of the message exchange with which to correlate.
     * @param data The data to correlate with the exchange.
     */
    public void correlate(String exchangeId, Object data);
    
    /**
     * Decorrelates component data from a message exchange.
     * 
     * @param exchangeId The id of the message exchange.
     * @return The correlated data, if any, or <code>null</code>.
     */
    public Object decorrelate(String exchangeId);
    
    /**
     * Fetches correlated component data.
     * 
     * @param exchangeId The specified exchange id.
     * @return Correlated component data or <code>null</code>.
     */
    public Object lookup(String exchangeId);
}
