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
 * @(#)DefaultCRLInOptionalOut.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.exchange.impl;

import javax.jbi.messaging.InOptionalOut;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;

import com.sun.jbi.crl.mep.ManagerContext;
import com.sun.jbi.crl.mep.exchange.CRLInOptionalOut;

/**
 * Default implementation of {@link CRLInOptionalOut}.
 * 
 * @author Kevan Simpson
 */
public class DefaultCRLInOptionalOut extends AbstractCRLMessageExchange 
                                     implements CRLInOptionalOut {
    public DefaultCRLInOptionalOut(ManagerContext ctx, InOptionalOut msg) {
        super(ctx, msg);
    }

    /** @see javax.jbi.messaging.InOptionalOut#getInMessage() */
    public NormalizedMessage getInMessage() {
        return getInOptionalOut().getInMessage();
    }

    /** @see javax.jbi.messaging.InOptionalOut#getOutMessage() */
    public NormalizedMessage getOutMessage() {
        return getInOptionalOut().getOutMessage();
    }

    /** @see javax.jbi.messaging.InOptionalOut#setInMessage(javax.jbi.messaging.NormalizedMessage) */
    public void setInMessage(NormalizedMessage msg) throws MessagingException {
        getInOptionalOut().setInMessage(msg);
    }

    /** @see javax.jbi.messaging.InOptionalOut#setOutMessage(javax.jbi.messaging.NormalizedMessage) */
    public void setOutMessage(NormalizedMessage msg) throws MessagingException {
        getInOptionalOut().setOutMessage(msg);
    }

    protected InOptionalOut getInOptionalOut() {
        return (InOptionalOut) getMessageExchange();
    }
}
