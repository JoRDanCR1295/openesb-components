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
 * @(#)ThrottlingFilter.java
 *
 * Copyright 2004-2011 Open ESB Community
 *
 * END_HEADER - DO NOT EDIT
 */


/**
 *
 * This class listens to DeliveryChannel to accept Message Exchanges and
 * schedule them for routing (making decision whether this ME is inbound and
 * should be processed with a BPEL process or it's outbound one)
 *
 * @author Alexander Lomov
 */

package com.sun.jbi.engine.bpel.threads;

import com.sun.jbi.engine.bpel.core.bpel.engine.execution.BPELSEMessageExchangeRouter;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessagingException;


public class BPELSEChannelAcceptor extends Thread {
    
    private volatile boolean run;
    private DeliveryChannel channel;
    private String name;
    private static final Logger LOGGER = Logger.getLogger(BPELSEChannelAcceptor.class.getName());
    
    
    public BPELSEChannelAcceptor(DeliveryChannel channel, String name){
        if (channel == null)
            throw new RuntimeException("DeliveryChannel cannot be null");

        this.channel = channel;
        this.name = name;
    }
    
    @Override
    public void run(){
        run = true;

        while(run){
            MessageExchange me = null;
            try {
                me = channel.accept();
                if (me == null)
                    continue;
            } catch (MessagingException ex) {
                LOGGER.log(Level.WARNING, "MessagingException at BPELSEChannelAcceptor " + name, ex);
            }

            BPELSEMessageExchangeRouter meRouter = new BPELSEMessageExchangeRouter(me);
        }
    }
}
