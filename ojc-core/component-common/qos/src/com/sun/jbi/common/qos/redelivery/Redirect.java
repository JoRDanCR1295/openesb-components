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
 * @(#)ServiceOperation.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.redelivery;

import javax.xml.namespace.QName;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.util.Util;

/**
 * Represents a endpoint destination for a message exchange whose retry
 * attempts have been exhausted.
 * 
 * @author Kevan Simpson
 */
public class Redirect {
    private QName mOperation;
    private EndpointInfo mEndpoint;
    
    public Redirect(EndpointInfo endpt, String operation) {
        this(endpt, (Util.isEmpty(operation) ? null : QName.valueOf(operation))); 
    }

    public Redirect(EndpointInfo endpt, QName operation) {
        mEndpoint = endpt;
        if (operation != null && Util.isEmpty(operation.getLocalPart())) {
            // no op name specified
            mOperation = null;
        }
        else {
            mOperation = operation;
        }
    }

    /**
     * @return the operation
     */
    public QName getOperation() {
        return mOperation;
    }

    /**
     * @param operation the operation to set
     */
    protected void setOperation(QName operation) {
        mOperation = operation;
    }

    /**
     * @return the endpoint
     */
    public EndpointInfo getEndpoint() {
        return mEndpoint;
    }

    /**
     * @param endpoint the endpoint to set
     */
    protected void setEndpoint(EndpointInfo endpoint) {
        mEndpoint = endpoint;
    }
}
