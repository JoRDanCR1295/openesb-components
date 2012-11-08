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
 * @(#)DefaultCRLRobustInOnly.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.exchange.impl;

import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.RobustInOnly;

import com.sun.jbi.crl.mep.ManagerContext;
import com.sun.jbi.crl.mep.exchange.CRLRobustInOnly;

/**
 * Default implementation of {@link CRLRobustInOnly}.
 * 
 * @author Kevan Simpson
 */
public class DefaultCRLRobustInOnly extends AbstractCRLMessageExchange 
                                    implements CRLRobustInOnly {
    public DefaultCRLRobustInOnly(ManagerContext ctx, RobustInOnly msg) {
        super(ctx, msg);
    }

    /** @see javax.jbi.messaging.RobustInOnly#getInMessage() */
    public NormalizedMessage getInMessage() {
        return getRobustInOnly().getInMessage();
    }

    /** @see javax.jbi.messaging.RobustInOnly#setInMessage(javax.jbi.messaging.NormalizedMessage) */
    public void setInMessage(NormalizedMessage msg) throws MessagingException {
        getRobustInOnly().setInMessage(msg);
    }

    protected RobustInOnly getRobustInOnly() {
        return (RobustInOnly) getMessageExchange();
    }
}
