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
 * @(#)MessageCatalog.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.runtime.messages;

import java.util.HashMap;
import java.util.Map;
import java.util.MissingResourceException;

/**
 * Collection of defined informational, warning and error messages for
 * Cobol Copybook Converter subsystems.
 *
 * @author nang
 * @version $Revision: 1.2 $
 */
public class MessageCatalog {

    private MessageCatalog() {
    }

    /**
     * Obtain a Message.
     *
     * @param id Message identity, must match a key in
     *           com.sun.encoder.coco.runtime.messages.Messages.properties.
     *
     * @throws NullPointerException     if <code>id</code> is <code>null</code>
     * @throws MissingResourceException if <code>id</code> cannot be resolved
     */ 
    public static Message getMessage(String id) {
        Message msg;

        synchronized (cCATALOG) {
            msg = (Message) cCATALOG.mMsgMap.get(id);
            if (msg == null) {
                msg = new MessageImpl(id);
                cCATALOG.mMsgMap.put(id, msg);
            }
        }

        return msg;
    }
    private static final MessageCatalog cCATALOG = new MessageCatalog();
    private final Map<String, Message> mMsgMap = new HashMap<String, Message>();
}

// FINIS $RCSfile: MessageCatalog.java,v $
