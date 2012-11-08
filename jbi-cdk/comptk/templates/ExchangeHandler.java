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
 * @(#)%CLASS_PREFIX%ExchangeHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package %PKG%;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.common.classloader.CustomClassLoaderUtil.SwitchType;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.common.qos.messaging.ExchangeUtil;
import com.sun.jbi.common.qos.messaging.ExchangeUtil.FaultCode;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.common.util.Util;

import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.jbi.component.toolkit.lifecycle.impl.AbstractExchangeHandler;
import com.sun.jbi.component.toolkit.util.ExchangePattern;
import com.sun.jbi.component.toolkit.util.PatternRoleStatus;

/**
 * Handles exchanges for %PROJ_SHORT_NAME% %COMP_TYPE_DESC%.
 * @author %AUTHOR%
 */
public class %CLASS_PREFIX%ExchangeHandler extends AbstractExchangeHandler {
    /** Constructs an {@link ExchangeHandler} for %PROJ_SHORT_NAME% %COMP_TYPE_DESC%. */
    public %CLASS_PREFIX%ExchangeHandler(ComponentContext ctx) {
        super(ctx);
    }
    
    /** @see com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler#handleExchange(javax.jbi.messaging.MessageExchange) */
    public void handleExchange(MessageExchange msg) throws JBIException {
        try {
            /* 
             * An enum to help direct the message exchange handling,
             * PatternRoleStatus clearly identifies the state of the exchange.
             * Please refer to Section 5.4.2 of the JBI spec for more details.
             */
            PatternRoleStatus prs = PatternRoleStatus.valueOf(msg);
            switch (prs) {
                case IN_OUT_PROVIDER_ACTIVE: {
                    // TODO (required) Handle incoming InOut request
                    break;
                }
                case IN_OUT_PROVIDER_DONE:
                case IN_OUT_PROVIDER_ERROR: {
                    /*
                     * Exchange is complete; perhaps log the ERROR status...
                     */
                    // TODO (optional) Handle ERROR response
                    break;
                }
                /*
                 * This case only applies to components which send invocations
                 * through the NMR, where component acts as a service consumer
                 */
                case IN_OUT_CONSUMER_ACTIVE: {
                    if (msg.getFault() != null) {
                        // TODO Handle fault; remove this block if component doesn't handle faults 
                    }
                    else {  // ACTIVE, should never be DONE
                        // send ACK to consumer to complete InOut exchange
                        msg.setStatus(ExchangeStatus.DONE);
                        send(msg);

                        // TODO (required) Continue processing after invoke completes
                    }
                    break;
                }
                case IN_OUT_CONSUMER_ERROR: {
                    /*
                     * This implements basic Redelivery for synchronous InOut
                     * exchanges, meaning ACK is not sent until exchange completes
                     */
                    // TODO (optional) Enhance or remove as needed 
                    RedeliveryStatus status = Redelivery.getRedeliveryStatus(msg);
                    if (status != null) {
                        if (!status.hasFailed()) {
                            // TODO (required) Create new exchange and resend
                            return;
                        }
                    }
                    /*
                     * Otherwise, relay error back to original consumer.
                     * Do NOT send THIS exhange as it's terminated by ERROR status
                     * Relay ERROR status to consumer of service this component provisions
                     */
                    // TODO (required) Relay ERROR back to consumer of provisioned service
                    break;
                }
                case IN_ONLY_PROVIDER_ACTIVE: { // never DONE or ERROR
                    // TODO (required) Handle incoming InOnly request
                    // TODO (required) Send status response: DONE or ERROR
                    break;
                }
                case IN_ONLY_CONSUMER_DONE: {    // always a status response, DONE or ERROR
                    // exchange is complete, do not respond 
                    // TODO (required) Handle InOnly response
                    break;
                }
                case IN_ONLY_CONSUMER_ERROR: {
                    /*
                     * This implements basic Redelivery for synchronous InOnly
                     * exchanges, meaning ACK is not sent until exchange completes
                     */
                    // TODO (optional) Enhance or remove as needed 
                    RedeliveryStatus status = Redelivery.getRedeliveryStatus(msg);
                    if (status != null) {
                        if (!status.hasFailed()) {
                            // TODO (required) Create new exchange and resend
                            return;
                        }
                    }

                    // TODO (required) Handle InOnly response with ERROR status

                    break;
                }
                default: {  // unsupported pattern
                    String err = I18n.loc(
                            "%LOG_PREFIX%-7001: {0} does not support {1} exchange pattern", 
                            this.getClass().getSimpleName(), 
                            String.valueOf(ExchangePattern.valueOf(msg)));
                    log().severe(err);
                    if (ExchangeStatus.ACTIVE.equals(msg.getStatus())) {
                        ExchangeUtil.setErrorData(msg, err, FaultCode.Client, null, getComponentName());
                        sendError(msg, new Exception(err));
                    }
                    else if (ExchangeStatus.ERROR.equals(msg.getStatus())) {
                        // TODO (optional) Log termination of exchange
                    }
                }
            }
        }
        catch (Exception e) {
            // processing exceptions are handled, but anything here means we can't send
            // log and throw
            throw error(I18n.loc("%LOG_PREFIX%-7002: Failed to complete exchange({0}) for endpoint({1}-{2}): {3}",
                                 msg.getExchangeId(), msg.getEndpoint().getServiceName(),
                                 msg.getEndpoint().getEndpointName(), e.getMessage()), 
                        e);
        }
    }
}
