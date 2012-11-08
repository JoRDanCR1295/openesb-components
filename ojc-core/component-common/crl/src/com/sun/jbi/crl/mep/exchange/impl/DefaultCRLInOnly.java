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
 * @(#)DefaultCRLInOnly.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.exchange.impl;

import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.crl.mep.ManagerContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;

/**
 * Default implementation of {@link CRLInOnly}.
 * 
 * @author Kevan Simpson
 */
public class DefaultCRLInOnly extends AbstractCRLMessageExchange implements CRLInOnly {
    public DefaultCRLInOnly(ManagerContext ctx, InOnly msg) {
        super(ctx, msg);
    }

	public DefaultCRLInOnly(ManagerContext ctx, InOnly msg, EndpointInfo info) {
        super(ctx, msg, info);
    }
    
    /** @see javax.jbi.messaging.InOnly#getInMessage() */
    public NormalizedMessage getInMessage() {
        return getInOnly().getInMessage();
    }

    /** @see javax.jbi.messaging.InOnly#setInMessage(javax.jbi.messaging.NormalizedMessage) */
    public void setInMessage(NormalizedMessage msg) throws MessagingException {
        getInOnly().setInMessage(msg);
    }

    protected InOnly getInOnly() {
        return (InOnly) getMessageExchange();
    }
}
