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
 * @(#)DefaultLogClient.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.client.impl;

import com.sun.jbi.common.tale.client.TaleClient;
import com.sun.jbi.common.tale.client.LogClient;
import com.sun.jbi.common.tale.core.domain.TaleRequest;
import com.sun.jbi.common.tale.core.domain.Payload;
import com.sun.jbi.common.tale.core.domain.service.TaleService;
import com.sun.jbi.common.tale.core.util.TaleException;

/**
 * 
 * @author Kevan Simpson
 */
public class DefaultLogClient extends AbstractConvenienceClient 
                              implements LogClient {
    /**
     * @param parent
     * @param service
     */
    public DefaultLogClient(TaleClient parent, TaleService service) {
        super(parent, service);
    }

    public void sendLog(int code, String details, String displayMessage) throws TaleException {
        sendLog(code, details, displayMessage, null);
    }

    /** @see com.sun.jbi.common.tale.client.LogClient#sendLog(java.lang.String, java.lang.String, java.lang.String, java.lang.String) */
    public void sendLog(int code, String details, String displayMessage,
                        String payload) throws TaleException {
        TaleRequest req = newRequest();
        req.setCode(code);
        req.setDetails(details);
        req.setDisplayMessage(displayMessage);
        if (payload == null) {
            req.setPayload(null);
        }
        else {
            if (req.getPayload() == null) {
                req.setPayload(new Payload());
            }
            req.getPayload().setPayloadMessage(payload);
        }
        
        getALEService().execute(req);
    }

}
