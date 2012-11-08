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
 * @(#)ASynchCallTracker.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.res.impl;

import javax.jbi.messaging.MessageExchange;

/**
 *
 * @author gpatil
 */
public interface CallTracker {
    /**
     * Consumer calls before making asynchronous call.
     */
    public void sentAsynchInMsg(MessageExchange provME, MessageExchange consME);
    /**
     * Consumer calls after making unsuccessful asynchronous call.
     */
    public void setSendASynchInMsgError(MessageExchange consME);
    
    public boolean isValid2CallASynchInOnly();

    public boolean isValid2CallASynchInOut();

    public boolean isValid2CallASynch();

    public boolean isExecutingOnDone();

    public String getUniqueMsgId();

    public void updateActiveSynchCalls(boolean increment);

    /**
     * Returns True when TXN is suspended and propagated for consuming ME.
     * If true, associate passed MessageExchange consME for propagating txn.
     *
     * Subsequent call will return false.
     * 
     * @param consME
     * @return
     */
    public boolean isOkToPropagateTxn(MessageExchange consME);

    /**
     * Returns True when TXN is suspended and propagated for consuming ME.
     *
     * @return
     */
//    public boolean isOkToPropagateTxn();

    public boolean isOkToResumeTxn(MessageExchange consME);
}
