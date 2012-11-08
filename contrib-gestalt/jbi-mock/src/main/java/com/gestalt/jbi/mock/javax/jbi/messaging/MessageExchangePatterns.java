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
 * MessageExchangePatterns.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.mock.javax.jbi.messaging;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.net.URI;
import java.net.URISyntaxException;


/**
 * Utility class used to hold the 4 MEP's
 */
public class MessageExchangePatterns {
    private static Log log = LogFactory.getLog(MessageExchangePatterns.class);
    public static final String IN_OUT = "http://www.w3.org/2004/08/wsdl/in-out";

    public static URI getInOutURI() {
        try {
            return new URI(IN_OUT);
        } catch (URISyntaxException e) {
            log.error(e);

            return null;
        }
    }
}
