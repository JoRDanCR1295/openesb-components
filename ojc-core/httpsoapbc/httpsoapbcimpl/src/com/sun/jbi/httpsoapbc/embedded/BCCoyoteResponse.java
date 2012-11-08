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
 * @(#)BCCoyoteResponse.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.embedded;

import com.sun.jbi.internationalization.Messages;

import java.util.logging.Logger;

import org.apache.catalina.Context;
import org.apache.coyote.tomcat5.CoyoteResponse;
import org.apache.coyote.tomcat5.CoyoteOutputStream;
import org.apache.coyote.tomcat5.CoyoteWriter;
import org.apache.coyote.tomcat5.OutputBuffer;

/**
 * Customized version of the Tomcat 5 CoyoteResponse
 */
public class BCCoyoteResponse extends CoyoteResponse {

    //private static Logger logger = LogDomains.getLogger(LogDomains.PWC_LOGGER);
    private final static Logger logger = Messages.getLogger(BCCoyoteResponse.class);

    
    /*
     * Constructor.
     */
    public BCCoyoteResponse(boolean chunkingDisabled) {
	super(chunkingDisabled);
    }

}
