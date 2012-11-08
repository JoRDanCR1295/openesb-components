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
 * @(#)CRLInOut.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.exchange;

import javax.jbi.JBIException;
import javax.jbi.messaging.InOut;
import javax.xml.transform.Source;

/**
 * Customized message exchange view for InOut pattern.
 * 
 * @author Kevan Simpson
 */
public interface CRLInOut extends CRLMessageExchange, InOut {
    /**
     * Sets the reply content via 
     * {@link InOut#setOutMessage(javax.jbi.messaging.NormalizedMessage)}
     * and sends to the NMR.
     * 
     * @param src The reply content.
     * @throws JBIException if an error occurs sending reply.
     */
    public void reply(Source src) throws JBIException;
}
