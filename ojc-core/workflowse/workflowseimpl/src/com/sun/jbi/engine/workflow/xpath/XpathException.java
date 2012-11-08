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
 * @(#)ClientApiException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.xpath;

import com.sun.jbi.engine.workflow.WorkflowException;

/**
 *
 * @author radval
 */
public class XpathException extends WorkflowException {

    public XpathException(String msg, Throwable th) {
		super(msg, th);
		// TODO Auto-generated constructor stub
	}

	public XpathException(String msg) {
		super(msg);
		// TODO Auto-generated constructor stub
	}

	public XpathException(String faultCode, String msg, Throwable t) {
        super(faultCode, msg, t);
        // TODO Auto-generated constructor stub
    }

    public XpathException(String faultCode, String msg) {
        super(faultCode, msg);
        // TODO Auto-generated constructor stub
    }

    public XpathException(Throwable t) {
        super(t);
        // TODO Auto-generated constructor stub
    }
}