/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.  
 *
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with 
 * your own identifying information: 
 * "Portions Copyrighted [year] [name of copyright owner]"
 */

/*
 * @(#)TextListener.java	1.4 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

import javax.jms.*;

/**
 * The TextListener class implements the MessageListener interface by 
 * defining an onMessage method that displays the contents of a TextMessage.
 * <p>
 * This class acts as the listener for the AsynchQueueReceiver class.
 *
 * @author Kim Haase
 * @version 1.5, 08/09/00
 */
public class TextListener implements MessageListener {
    final SampleUtilities.DoneLatch  monitor = 
        new SampleUtilities.DoneLatch();

    /**
     * Casts the message to a TextMessage and displays its text.
     * A non-text message is interpreted as the end of the message 
     * stream, and the message listener sets its monitor state to all 
     * done processing messages.
     *
     * @param message	the incoming message
     */
    public void onMessage(Message message) {
        if (message instanceof TextMessage) {
            TextMessage  msg = (TextMessage) message;
            
            try {
                System.out.println("Reading message: " + msg.getText());
            } catch (JMSException e) {
                System.out.println("Exception in onMessage(): " + e.toString());
            }
        } else {
            monitor.allDone();
        }
    }
}
