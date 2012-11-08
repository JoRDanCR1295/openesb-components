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
 * @(#)InboundConsumer.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.binding.email.protocol.receive;


import com.sun.jbi.binding.email.protocol.receive.pop3.POP3MessageReceiver;
import com.sun.jbi.binding.email.protocol.receive.pop3.POP3Endpoint;
import com.sun.jbi.binding.email.protocol.receive.imap.IMAPMessageReceiver;
import com.sun.jbi.binding.email.protocol.receive.imap.IMAPEndpoint;
import javax.jbi.JBIException;

import com.sun.jbi.binding.email.EmailBCEndpoint;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 */
public class InboundConsumer {
    private EmailBCEndpoint mEndpoint = null;
    private MessageReceiver mReceiver = null;

    public InboundConsumer(EmailBCEndpoint emailEndpoint){
        this.mEndpoint = emailEndpoint;
    }

    public void startConsuming() throws JBIException {
        if (this.mEndpoint instanceof IMAPEndpoint){
            this.mReceiver = new IMAPMessageReceiver(this.mEndpoint);
        } else if (this.mEndpoint instanceof POP3Endpoint){
            this.mReceiver = new POP3MessageReceiver(this.mEndpoint);
        }
        this.mReceiver.startReceiving();

        String threadName = "EmailBC.InboundConsumer." + this.mEndpoint.getInfo().getEndpointName();
        new Thread(this.mReceiver, threadName).start();

    }

    public void stopConsuming() throws JBIException{
        this.mReceiver.stopReceiving();
    }

}
