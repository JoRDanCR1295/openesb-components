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
 * @(#)Response.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator;

import java.util.List;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 *
 * @author Bing Lu
 */
public class Response {
    private Request mRequest;
    private Element mXml;
    private List<Object[]> mRowList;
    
    public Response(Request request, Element xml) {
        mRequest = request;
        mXml = xml;
        mRowList = request.getInvoker().getRowList(this);
    }

    public Request getRequest() {
        return mRequest;
    }

    public Element getXml() {
        return mXml;
    }
    
    public int getSize() {
        return mRowList.size();
    }
    
    public Object[] getRow(int i) {
        return mRowList.get(i);
    }
    
}
