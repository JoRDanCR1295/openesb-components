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
 * @(#)MessageContainerFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine;

import javax.transaction.Transaction;

import com.sun.jbi.engine.bpel.core.bpel.engine.impl.MessageContainerImpl;


/**
 * message container factory
 *
 * @author Sun Microsystems
 */
public class MessageContainerFactory {
    /**
     * Creates a new instance of MessageContainerFactory
     */
    private MessageContainerFactory() {
    }

    /**
     * creates status message container
     *
     * @param id message ID
     * @param content content
     *
     * @return MessageContainer status message container for done
     */
    public static MessageContainer createDoneStatus(String id, Object reliableMesgId, Transaction transaction){
        return new MessageContainerImpl(MessageContainer.STATUS_DONE, id, null,
                reliableMesgId, transaction);
    }

    /**
     * creates status message container
     *
     * @param id message ID
     * @param content content
     *
     * @return MessageContainer status message container for error
     */
    public static MessageContainer createErrorStatus(String id, Object content, Object reliableMesgId, 
            Transaction transaction) {
        return new MessageContainerImpl(MessageContainer.STATUS_ERROR, id,
                content, reliableMesgId, transaction);
    }
    
    /**
     * creats data message container
     *
     * @param id message id
     * @param content content
     * @param reliableMesgId reliable messaging ID
     *
     * @return MessageContainer data message container
     */
    public static MessageContainer createMessage(String id, Object content,
        Object reliableMesgId, Transaction transaction) {
        return new MessageContainerImpl(MessageContainer.MESSAGE, id, content,
            reliableMesgId, transaction);
    }

    public static MessageContainer createMessage(String id, Object content,
            MessageContainer request) {
            return new MessageContainerImpl(MessageContainer.MESSAGE, id, content, request);
        }
    
    /**
     * creats fault message container
     *
     * @param id message id
     * @param content content
     *
     * @return MessageContainer fault message container
     */
    public static MessageContainer createFault(String id, Object content) {
        return new MessageContainerImpl(MessageContainer.FAULT, id, content,
            null, null);
    }
    
    /**
     * Creates fault message container with transaction context.
     * the fault message happened in the bounds of a tranaction.
     * @param id
     * @param content
     * @param reliableMesgId
     * @param transaction
     * @return
     */
    public static MessageContainer createFault(String id, Object content
    			, Object reliableMesgId, Transaction transaction) {
        return new MessageContainerImpl(MessageContainer.FAULT, id, content,
        		reliableMesgId, transaction);
    }
    
}
