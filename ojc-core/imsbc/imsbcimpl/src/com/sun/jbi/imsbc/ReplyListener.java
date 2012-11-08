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

package com.sun.jbi.imsbc;

import javax.jbi.messaging.MessageExchange;


/**
 * Listener interface for processing reply.
 *
 * @author Sun Microsystems
 *
 */
public interface ReplyListener {
    /**
     * Process reply
     * @param me MessageExchange to process.
     * @returns A boolean true if successful processing of the reply; false otherwise.
     */
    public boolean onReply (MessageExchange me);
    
    /**
     * Get the request received time
     * @return Request invocation time.
     */
    public long getRequestInvocationTime();
}
