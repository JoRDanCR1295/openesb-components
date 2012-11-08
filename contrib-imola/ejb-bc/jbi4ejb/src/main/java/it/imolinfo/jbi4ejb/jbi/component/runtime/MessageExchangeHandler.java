/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * Copyright 2004-2006 Sun Microsystems, Inc. All Rights Reserved.
 */

/*
 * MessageExchangeHandler.java
 *
 */

package it.imolinfo.jbi4ejb.jbi.component.runtime;

import javax.jbi.messaging.MessageExchange;

/**
 * This interface is a Handler to perform message exchanges when the component
 * receives the MessageExchange object from the delivery channel.
 * Implemenation of this interface should implement the processing of
 * the active, error, done status of the MessageExchange object according to
 * the MEP for which the MessageExchange object is created.
 *
 * @author Sun Microsystems, Inc.
 */
public interface MessageExchangeHandler extends Runnable {
    /**
     * sets the MessageExchange object to be processed
     * @param msgExchange MessageExchange object.
     */
    void setMessageExchange(MessageExchange msgExchange);
    /**
     * command interface method which will be invoked to process the MessageExchange
     * object set using setMessageExchange.
     */
    void processMessageExchange();
    
}
