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
 * @(#)EmailHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.recoverytest;

import java.util.Date;
import java.util.Properties;

import javax.mail.Message;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

/**
 *  Class EmailHelper
 *
 *  Currently only supports one email in to address
 */
public class EmailHelper {
    
    public static void sendmail(Properties properties, String msgStr) throws Exception {
        Session session = Session.getInstance(properties);
        
        // Construct a MimeMessage
        Message msg = new MimeMessage(session);
        msg.setFrom(new InternetAddress(properties.getProperty("from")));
        msg.setRecipients(Message.RecipientType.TO,InternetAddress.parse(properties.getProperty("to"), false));
        
        // -- set a CC: or BCC:
        String cc  = properties.getProperty("cc");
        String bcc = properties.getProperty("bcc");
        if (cc != null &&  (!cc.trim().equals(""))) msg.setRecipients(Message.RecipientType.CC, 
                InternetAddress.parse(cc,  false));
        if (bcc != null && (!bcc.trim().equals(""))) msg.setRecipients(Message.RecipientType.BCC, 
                InternetAddress.parse(bcc, false));
        
        msg.setSubject(properties.getProperty("subject"));
        msg.setSentDate(new Date());
        msg.setText(properties.getProperty("body") + "\n" + msgStr);
        
        // Send the message.
        Transport.send(msg);
    }
}
