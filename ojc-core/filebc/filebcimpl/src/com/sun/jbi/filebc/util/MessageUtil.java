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
 * @(#)MessageUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.util;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.filebc.Endpoint;
import com.sun.jbi.filebc.FileComponentContext;
import com.sun.jbi.filebc.FileMeta;
import com.sun.jbi.filebc.InboundMessageProcessor;
import com.sun.jbi.internationalization.Messages;
import java.util.Set;
import javax.jbi.messaging.NormalizedMessage;

public class MessageUtil {

    public static final String PROP_FAULTCODE = "com.sun.jbi.crl.faultcode";
    public static final String PROP_FAULTSTRING = "com.sun.jbi.crl.faultstring";
    public static final String PROP_FAULTACTOR = "com.sun.jbi.crl.faultactor";
    public static final String PROP_FAULTDETAIL = "com.sun.jbi.crl.faultdetail";
    public static final String FAULTCODE_SERVER = "Server";
    public static final String FAULTCODE_CLIENT = "Client";
    private static final Messages mMessages =
            Messages.getMessages(MessageUtil.class);
    private static Logger mLogger = Messages.getLogger(InboundMessageProcessor.class);

    private MessageUtil() {}
    
    public static void checkAndGenerateInMemoryCRMP(MessageExchange mex, Endpoint ep) {
        // If the CRMP groupid and messageid properties are NOT set,
        // then generate GUID for groupid and in memory "one-up" for messageid
        // Note: groupid is random in this case, so there's really no grouping
        if (mex.getProperty(FileComponentContext.CRMP_GROUP_ID) == null ||
                mex.getProperty(FileComponentContext.CRMP_MESSAGE_ID) == null) {
            String groupId = generateGUID();
            String messageId = Long.toString(ep.getCRMPMessageId());
            mex.setProperty(FileComponentContext.CRMP_GROUP_ID, groupId);
            mex.setProperty(FileComponentContext.CRMP_MESSAGE_ID, messageId);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Generated: " +
                        FileComponentContext.CRMP_GROUP_ID + " with value [" +
                        groupId + "] and " +
                        FileComponentContext.CRMP_MESSAGE_ID + " with value [" +
                        messageId + "] for message exchange with ID [" + mex.getExchangeId() + "]");
            }
        }
    }

    public static String generateGUID() {
        return java.util.UUID.randomUUID().toString();
    }

    public static void setNMProperties(NormalizedMessage nm, FileMeta fileMeta) {
        Set<String> keys = fileMeta.getNMProperties().keySet();
        for (String key : keys) {
            if (nm.getProperty(key) == null) {
                //Incase this is a re-delivery message, it will already have NM properties copied
                //from the original message. Therefore set a NM property only if the message does
                //not already contain it.
                nm.setProperty(key, fileMeta.getNMProperty(key));
            }
        }
    }

    public static void setNMProperties(NormalizedMessage nm, String[] keys, String[] values) {
        for (int i = 0; i < keys.length; i++) {
            if (nm.getProperty(keys[i]) == null) {
                nm.setProperty(keys[i], values[i]);
            }
        }
    }
}
