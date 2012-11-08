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
 * @(#)ListenerMeta.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc;


/**
 * This is the class that encapsulates metadata
 * information about the message exchange listener
 *
 * @author  Venkat P
 *
 */
public class ListenerMeta {
    private long timestamp;
    private MessageExchangeReplyListener listener;

    protected ListenerMeta(final long timestamp, final MessageExchangeReplyListener listener) {
        this.timestamp = timestamp;
        this.listener = listener;
    }

    /**
     *
     * @return
     */
    protected long getRequestInvocationTime() {
        return timestamp;
    }

    /**
     *
     * @return
     */
    protected MessageExchangeReplyListener getMessageExchangeReplyListener() {
        return listener;
    }
}
