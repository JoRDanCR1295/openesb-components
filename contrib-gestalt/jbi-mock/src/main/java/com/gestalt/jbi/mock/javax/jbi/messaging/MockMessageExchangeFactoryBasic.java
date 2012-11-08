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
 * MockMessageExchangeFactoryBasic.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.mock.javax.jbi.messaging;

import javax.jbi.messaging.*;
import javax.xml.namespace.QName;
import java.net.URI;


public class MockMessageExchangeFactoryBasic
    implements MockMessageExchangeFactory {
    private InOut inOut;
    private InOnly inOnly;

    public MessageExchange createExchange(QName qName, QName qName1)
        throws MessagingException {
        return null;
    }

    public MessageExchange createExchange(URI uri) throws MessagingException {
        return null;
    }

    public InOnly createInOnlyExchange() throws MessagingException {
        return inOnly;
    }

    public InOptionalOut createInOptionalOutExchange()
        throws MessagingException {
        return null;
    }

    public InOut createInOutExchange() throws MessagingException {
        return inOut;
    }

    public RobustInOnly createRobustInOnlyExchange() throws MessagingException {
        return null;
    }

    public InOut getInOut() {
        return inOut;
    }

    public void setInOut(InOut inOut) {
        this.inOut = inOut;
    }

    public InOnly getInOnly() {
        return inOnly;
    }

    public void setInOnly(InOnly inOnly) {
        this.inOnly = inOnly;
    }
}
