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
 * @(#)DefaultCRLInOut.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.exchange.impl;

import javax.jbi.JBIException;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.transform.Source;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.crl.mep.ManagerContext;
import com.sun.jbi.crl.mep.exchange.CRLInOut;

/**
 * Default implementation of {@link CRLInOut}.
 * 
 * @author Kevan Simpson
 */
public class DefaultCRLInOut extends AbstractCRLMessageExchange implements CRLInOut {
    public DefaultCRLInOut(ManagerContext ctx, InOut msg) {
        super(ctx, msg);
    }

    public DefaultCRLInOut(ManagerContext ctx, InOut msg, EndpointInfo info) {
        super(ctx, msg, info);
    }

    /** @see com.sun.jbi.crl.mep.exchange.CRLInOut#reply(javax.xml.transform.Source) */
    public void reply(Source src) throws JBIException {
        NormalizedMessage xOut = createMessage();
        xOut.setContent(src);
        setOutMessage(xOut);
        send();
    }

    /** @see javax.jbi.messaging.InOut#getInMessage() */
    public NormalizedMessage getInMessage() {
        return getInOut().getInMessage();
    }

    /** @see javax.jbi.messaging.InOut#getOutMessage() */
    public NormalizedMessage getOutMessage() {
        return getInOut().getOutMessage();
    }

    /** @see javax.jbi.messaging.InOut#setInMessage(javax.jbi.messaging.NormalizedMessage) */
    public void setInMessage(NormalizedMessage msg) throws MessagingException {
        getInOut().setInMessage(msg);
    }

    /** @see javax.jbi.messaging.InOut#setOutMessage(javax.jbi.messaging.NormalizedMessage) */
    public void setOutMessage(NormalizedMessage msg) throws MessagingException {
        getInOut().setOutMessage(msg);
    }

    protected InOut getInOut() {
        return (InOut) getMessageExchange();
    }
}
