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
 * @(#)WebServiceContext.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Bing Lu
 */
public class WebServiceContext {
    private static WebServiceInvocationFactory mWsFactory;
    
    public static void setInvocationFactory(WebServiceInvocationFactory wsFactory) {
        mWsFactory = wsFactory;
    }
    
    private static WebServiceInvocationFactory getInvocationFactory() {
        return mWsFactory;
    }
    
    private static WebServiceContext mInstance = new WebServiceContext();
    
    public static WebServiceContext getInstance() {
        return mInstance;
    }
    
    public List<Response> invoke(List<Request> requestList) {
        List<Response> responseList = new ArrayList<Response>();
        for (int i = 0, I = requestList.size(); i < I; i++) {
            Request request = requestList.get(i);
            WebServiceInvocation wsi = getInvocationFactory().newInvocation(request);
            wsi.invoke();
            responseList.add(wsi.getResponse());
        }
        return responseList;
    }
}
