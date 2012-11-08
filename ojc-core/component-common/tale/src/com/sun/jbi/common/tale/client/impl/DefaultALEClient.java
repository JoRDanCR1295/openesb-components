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
 * @(#)DefaultALEClient.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.client.impl;

import java.util.HashMap;
import java.util.Map;

import com.sun.jbi.common.tale.client.TaleClient;
import com.sun.jbi.common.tale.client.AlertClient;
import com.sun.jbi.common.tale.client.ErrorClient;
import com.sun.jbi.common.tale.client.LogClient;
import com.sun.jbi.common.tale.core.domain.TaleDomain;
import com.sun.jbi.common.tale.core.domain.TaleRequest;
import com.sun.jbi.common.tale.core.domain.service.LoggingService;
import com.sun.jbi.common.tale.core.util.TaleException;
import com.sun.jbi.common.util.Util;

/**
 * Default implementation of {@link TaleClient}.
 * @author Kevan Simpson
 */
public class DefaultALEClient implements TaleClient {
    private Map<String, TaleRequest> mTemplates;
    private AlertClient mAlertClient;
    private LogClient mLogClient;
    private ErrorClient mErrorClient;
    
    public DefaultALEClient(TaleDomain domain) {
        mTemplates = new HashMap<String, TaleRequest>();
        mTemplates.put(DEFAULT_TEMPLATE, new TaleRequest());
        mAlertClient = null;    // TODO implement
        mLogClient = new DefaultLogClient(this, new LoggingService(domain));
        mErrorClient = null;    // TODO implement
    }
    
    /** @see com.sun.jbi.common.tale.client.TaleClient#addRequestTemplate(java.lang.String, com.sun.jbi.common.tale.core.domain.TaleRequest) */
    public void addRequestTemplate(String name, TaleRequest template) {
        if (!Util.isEmpty(name) && template != null) {
            mTemplates.put(name, template);
        }
    }

    /** @see com.sun.jbi.common.tale.client.TaleClient#getAlertClient() */
    public AlertClient getAlertClient() {
        return mAlertClient;
    }

    /** @see com.sun.jbi.common.tale.client.TaleClient#getErrorClient() */
    public ErrorClient getErrorClient() {
        return mErrorClient;
    }

    /** @see com.sun.jbi.common.tale.client.TaleClient#getLogClient() */
    public LogClient getLogClient() {
        return mLogClient;
    }

    /** @see com.sun.jbi.common.tale.client.TaleClient#getRequestTemplate(java.lang.String) */
    public TaleRequest getRequestTemplate(String name) {
        return mTemplates.get(name);
    }

    /** @see com.sun.jbi.common.tale.client.TaleClient#newRequest() */
    public TaleRequest newRequest() {
        return newRequest(DEFAULT_TEMPLATE);
    }

    /** @see com.sun.jbi.common.tale.client.TaleClient#newRequest(java.lang.String) */
    public TaleRequest newRequest(String templateName) {
        return TaleRequest.copy(getRequestTemplate(templateName));
    }

    /** @see com.sun.jbi.common.tale.client.TaleClient#removeRequestTemplate(java.lang.String) */
    public TaleRequest removeRequestTemplate(String name) {
        return mTemplates.remove(name);
    }

    /** @see com.sun.jbi.common.tale.client.TaleClient#sendAlert(com.sun.jbi.common.tale.core.domain.TaleRequest) */
    public void sendAlert(TaleRequest request) throws TaleException {
        // TODO is TaleRequest the correct input for AlertService?
        if (request != null) {
            getAlertClient().getALEService().execute(request);
        }
    }

    /** @see com.sun.jbi.common.tale.client.TaleClient#sendError(com.sun.jbi.common.tale.core.domain.TaleRequest) */
    public void sendError(TaleRequest request) throws TaleException {
        // TODO is TaleRequest the correct input for ErrorService?
        if (request != null) {
            getLogClient().getALEService().execute(request);
        }
    }

    /** @see com.sun.jbi.common.tale.client.TaleClient#sendLog(com.sun.jbi.common.tale.core.domain.TaleRequest) */
    public void sendLog(TaleRequest request) throws TaleException {
        if (request != null) {
            getLogClient().getALEService().execute(request);
        }
    }

}
