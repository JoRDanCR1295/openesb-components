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
 * @(#)XsltSendFailureListener.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.runtime;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;

import com.sun.jbi.common.qos.messaging.ExchangeUtil;
import com.sun.jbi.common.qos.messaging.SendFailureListener;
import com.sun.jbi.common.qos.messaging.ExchangeUtil.FaultCode;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.transform.I18n;
import com.sun.transform.engine.runtime.ProcessInstance;

/**
 * Handles channel send failures that cannot be returned to component due
 * to the asynchronous nature of message exchange patterns.
 * 
 * @author Kevan Simpson
 */
public class TransformSendFailureListener implements SendFailureListener {
    private static Logger mLogger = 
            Logger.getLogger(TransformSendFailureListener.class.getName());
    
    private ManagerContext mContext;

    public TransformSendFailureListener(ManagerContext ctx) {
        mContext = ctx;
    }
    
    /** @see com.sun.jbi.common.qos.messaging.SendFailureListener#handleSendFailure(javax.jbi.messaging.MessagingException, javax.jbi.messaging.MessageExchange) */
    public void handleSendFailure(MessagingException error, MessageExchange mex) {
        ProcessInstance proc = (ProcessInstance)    // decorrelate process
                mContext.getCorrelationMap().remove(mex.getExchangeId());
        if (proc != null) {
            try {
                MessageExchange initiatingMsg = proc.getMessageExchange();
                ExchangeUtil.setErrorData(initiatingMsg, 
                                          error.getMessage(), 
                                          FaultCode.Server, 
                                          null, 
                                          mContext.getComponentContext().getComponentName());
                initiatingMsg.setError(error);
                initiatingMsg.setStatus(ExchangeStatus.ERROR);
                mContext.getMessagingChannel().send(initiatingMsg);
            }
            catch (MessagingException fubar) {
                mLogger.log(Level.SEVERE, 
                            I18n.loc("TRANSL-7003: Failure to respond to invoke error, transformation process aborted: {0}", 
                                     fubar.getMessage()), 
                            fubar);
                // TODO send alert using ALEClient 
            }
        }
        // else what?  FOOBAR
    }
}
