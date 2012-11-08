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
 * @(#)MessagListenerEndpointFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.jca;

import javax.transaction.TransactionManager;
import javax.transaction.xa.XAResource;

import javax.resource.spi.UnavailableException;

import javax.resource.spi.endpoint.MessageEndpoint;
import javax.resource.spi.endpoint.MessageEndpointFactory;

/**
 *
 * Abstract Factory for MessageEndpoint/MessageListener endpoints.
 */
public abstract class MessagListenerEndpointFactory implements MessageEndpointFactory {

    private boolean isDeliveryTransacted  = false;
    private TransactionManager txManager = null;
    
    /** 
     * Creates a new instance of MessageProcessorEndpointFactory 
     */
    public MessagListenerEndpointFactory(boolean isDeliveryTransacted) {
        this.isDeliveryTransacted = isDeliveryTransacted;
    }
    
    public boolean isDeliveryTransacted(java.lang.reflect.Method method) throws NoSuchMethodException {
        // should put more checking..
        if (method.getName().indexOf("onMessage") > -1) {
            return isDeliveryTransacted;        
        } else {
            throw new NoSuchMethodException (method.getName());
        }
    }

}
