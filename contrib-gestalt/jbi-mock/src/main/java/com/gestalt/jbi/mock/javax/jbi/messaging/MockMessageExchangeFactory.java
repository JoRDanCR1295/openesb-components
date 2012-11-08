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
 * MockMessageExchangeFactory.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.mock.javax.jbi.messaging;

import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchangeFactory;


/**
 * Top level Mock interface for javax.jbi.MessageExchangeFactory.
 * This only exists because currently we are receiving a NullPointerException
 * in the ServiceMix class
 * MessageExchangeFactoryImpl.setDefaults(MessageExchangeImpl exchange)
 * when they attempt to call getContext().getActivationSpec(). Since
 * we are using our MockComponentContext, getContext() returns a null
 * and results in an NPE.
 * Once we hear back from ServiceMix, we should be able to eliminate this
 * class.
 * http://www.nabble.com/MessageExchangeFactoryImpl-NullPointerException-tf2064194.html
 * JIRA issue logged: https://issues.apache.org/activemq/browse/SM-518
 */
public interface MockMessageExchangeFactory extends MessageExchangeFactory {
    public InOut getInOut();

    public void setInOut(InOut inOut);
}
