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
 * @(#)LogClient.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.client;

import com.sun.jbi.common.tale.core.util.TaleException;

/**
 * Convenience ALE client for logging.
 * @author Kevan Simpson
 */
public interface LogClient extends ConvenienceClient {
    /**
     * Sends a log message.
     * @param code The log code.
     * @param details The details to log.
     * @param displayMessage The display message.
     * @throws TaleException if an error occurs sending log message.
     */
    public void sendLog(int code, String details, String displayMessage)
            throws TaleException;

    /**
     * Sends a log message.
     * @param code The log code.
     * @param details The details to log.
     * @param displayMessage The display message.
     * @param payload The payload message to log.
     * @throws TaleException if an error occurs sending log message.
     */
    public void sendLog(int code, String details, 
                        String displayMessage, String payload)
            throws TaleException;
}
