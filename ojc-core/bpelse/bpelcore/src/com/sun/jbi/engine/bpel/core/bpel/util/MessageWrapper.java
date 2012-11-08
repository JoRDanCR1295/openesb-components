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
 * @(#)MessageWrapper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.util;

import javax.wsdl.Message;

import com.ibm.wsdl.MessageImpl;

/**
 * WSDL4J wrapper around WSDLMessage
 *
 * Ensures that when used in a comparison/hashmap,
 * the underlying WSDLMessage instance counts
 *
 * @author Sun Microsystems
 * @deprecated
 */
public class MessageWrapper extends MessageImpl {
    /**
     * WSDL message instance to wrap in wsdl4j style. 
     * Final to ensure consistent hashcode/equals.
     */
    final Message message;

    /**
     * @param messageToWrap the WSDLMessage instance to wrap in a wsdl4j Message wrapper
     */
    public MessageWrapper(Message messageToWrap) {
        message = messageToWrap;
    }

    /** 
     * Implement equals based on the wrapped instance
     */
    public boolean equals (Object o) {
        if ((o != null) && (o.getClass().equals(this.getClass()))) {
            return (((MessageWrapper) o).message.equals(this.message));
        }
        return false;
    }     

    /**
     * Implement hashcode based on the wrapped instance
     */
    public int hashCode() {
        return message.hashCode();
    }
}
