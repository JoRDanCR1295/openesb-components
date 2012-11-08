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
 * MockInOutBasic.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.mock.javax.jbi.messaging;

import java.net.URI;

import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;


public class MockInOutBasic extends MockMessageExchangeBasic implements InOut {
    public NormalizedMessage getInMessage() {
        return getMessage("in");
    }

    public NormalizedMessage getOutMessage() {
        return getMessage("out");
    }

    public void setInMessage(NormalizedMessage normalizedMessage)
        throws MessagingException {
        setMessage(normalizedMessage, "in");
    }

    public void setOutMessage(NormalizedMessage normalizedMessage)
        throws MessagingException {
        setMessage(normalizedMessage, "out");
    }

    /** Get the pattern.
	 * @return the message exchange pattern.
	 */
	    public URI getPattern()
	    {
	        return MessageExchangePatterns.getInOutURI();
    }
}
