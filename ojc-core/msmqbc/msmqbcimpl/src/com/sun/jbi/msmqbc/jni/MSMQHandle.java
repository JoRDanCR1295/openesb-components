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
 * @(#)MSMQHandle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.jni;

/**
 * This class represents storage for a C handle (i.e. QUEUEHANDLE, HANDLE for cursor, etc). No
 * automatic closure, deleting, or anything else - it is just a place to store the handle value.
 * 
 * @author Sun Microsystems
 */

import java.util.logging.Logger;
import java.util.logging.Level;

import com.sun.jbi.internationalization.Messages;

public class MSMQHandle {

    private static final Messages mMessages = Messages.getMessages(MSMQHandle.class);

    private static final Logger mLogger = Messages.getLogger(MSMQHandle.class);

    protected int m_handle = 0;

    public MSMQHandle(int h) {
        m_handle = h;
    }

    public MSMQHandle() {
        m_handle = 0;
    }

    public void debugPrintHandle() {
        mLogger.log(Level.INFO, mMessages.getString("MSMQHANDLE.VALUE", new Object[] { m_handle }));
    }
}
