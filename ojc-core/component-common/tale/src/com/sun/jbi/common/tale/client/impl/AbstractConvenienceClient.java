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
 * @(#)AbstractConvenienceClient.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.client.impl;

import com.sun.jbi.common.tale.client.TaleClient;
import com.sun.jbi.common.tale.client.ConvenienceClient;
import com.sun.jbi.common.tale.core.domain.TaleRequest;
import com.sun.jbi.common.tale.core.domain.service.TaleService;
import com.sun.jbi.common.util.Util;

/**
 * Abstract base class for convenience clients.
 * @author Kevan Simpson
 */
public abstract class AbstractConvenienceClient implements ConvenienceClient {
    private String mTemplateName = TaleClient.DEFAULT_TEMPLATE;
    private final TaleService mService;
    private final TaleClient mClient;
    
    protected AbstractConvenienceClient(TaleClient parent, TaleService service) {
        mClient = parent;
        mService = service;
    }
    
    /** @see com.sun.jbi.common.tale.client.ConvenienceClient#getALEService() */
    public TaleService getALEService() {
        return mService;
    }

    /** @see com.sun.jbi.common.tale.client.ConvenienceClient#getRequestTemplateName() */
    public String getRequestTemplateName() {
        return mTemplateName;
    }

    /** @see com.sun.jbi.common.tale.client.ConvenienceClient#setRequestTemplateName(java.lang.String) */
    public void setRequestTemplateName(String templateName) {
        mTemplateName = (Util.isEmpty(templateName)) 
                ? TaleClient.DEFAULT_TEMPLATE : templateName;
    }
    
    protected TaleRequest newRequest() {
        return getClient().newRequest(getRequestTemplateName());
    }
    
    protected TaleClient getClient() {
        return mClient;
    }
}
