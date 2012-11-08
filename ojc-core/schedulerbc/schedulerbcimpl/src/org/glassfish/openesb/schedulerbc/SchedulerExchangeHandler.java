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
 * @(#)SchedulerExchangeHandler.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.schedulerbc;

import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.component.toolkit.util.ExchangePattern;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.impl.AbstractExchangeHandler;
import java.util.StringTokenizer;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.NormalizedMessage;
import org.glassfish.openesb.schedulerbc.domain.SchedulerConstants;
import org.glassfish.openesb.schedulerbc.domain.SchedulerConstants.PatternRole;
import org.quartz.JobExecutionContext;

/**
 * Handles exchanges for Scheduler.
 * @author sunsoabi_edwong
 */
public class SchedulerExchangeHandler extends AbstractExchangeHandler
        implements SchedulerConstants {

    /** Constructs an {@link ExchangeHandler} for Scheduler. */
    public SchedulerExchangeHandler(ComponentContext ctx) {
        super(ctx);
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler#handleExchange(javax.jbi.messaging.MessageExchange) */
    public void handleExchange(MessageExchange me) throws JBIException {
        PatternRole key = PatternRole.toEnum(me);
        switch (key) {
        case IN_ONLY_CONSUMER:
            I18n.finer(log(), "SCHEDBC-2006: Handling received Message "//NOI18N
                    + "Exchange id={0}, role={1}, status={2}",          //NOI18N
                    me.getExchangeId(), me.getRole().toString(),
                    me.getStatus().toString());
            
            if (!ExchangeStatus.ERROR.equals(me.getStatus())) {
                notifyResult(me,
                        new Result(Redelivery.getRedeliveryStatus(me)));
            } else {
                RedeliveryStatus redeliveryStatus =
                        Redelivery.getRedeliveryStatus(me);
                if (redeliveryStatus != null) {
                    if (!redeliveryStatus.hasFailed()) {
                        resend(me, redeliveryStatus);
                    } else {
                        notifyResult(me, new Result(redeliveryStatus));
                    }
                } else {
                    notifyResult(me, new Result(me.getError()));
                }
            }
            break;
        default:
            break;
        }
    }

	protected void resend(MessageExchange me, RedeliveryStatus rs)
            throws JBIException {
	    // declared outside try for error message, if needed
	    JobExecutionContext jobExecContext = (JobExecutionContext)
                getContext().getCorrelationMap().remove(me.getExchangeId());
        ExchangePattern patt = ExchangePattern.valueOf(me);
        if ((null == jobExecContext) || (patt != ExchangePattern.IN_ONLY)) {
            return;
        }

        // create new message...
        InOnly orgInOnlyME = (InOnly) me;
        InOnly newInOnlyME = (InOnly) createExchange(patt);
        newInOnlyME.setEndpoint(orgInOnlyME.getEndpoint());
        newInOnlyME.setOperation(orgInOnlyME.getOperation());
        NormalizedMessage newInOnlyNM = newInOnlyME.createMessage();
        newInOnlyME.setInMessage(newInOnlyNM);
        newInOnlyNM.setContent(orgInOnlyME.getInMessage().getContent());

        // propagate nm properties from orig message to new exchange
        String nmPropNames = (String) jobExecContext.getMergedJobDataMap()
                .get(REDELIVERY_NM_PROPS);
        StringTokenizer strToknzr = new StringTokenizer(nmPropNames,
                REDELIVERY_NM_PROPS_DELIM);
        while (strToknzr.hasMoreTokens()) {
            String key = strToknzr.nextToken();
            newInOnlyME.setProperty(key, me.getProperty(key));
        }
        Redelivery.setUniqueId(newInOnlyME, Redelivery.getUniqueId(me));

        // recorrelate process with new exchange and activity unit...
        getContext().getCorrelationMap().put(newInOnlyME.getExchangeId(),
                jobExecContext);

        // and resend content...
        if (I18n.finerLoggable(log())) {
            String attempt = (rs.getRemainingRetries() != -1)
                ? new StringBuilder("(")                                //NOI18N
                    .append(rs.getRemainingRetries())
                    .append(" more attempts left)").toString()          //NOI18N
                : "indefinitely";                                       //NOI18N
            I18n.finer(log(), "SCHEDBC-2007: Redelivering {0} with "    //NOI18N
                    + "Message Exchange id={1}, role={2}, status={3}",  //NOI18N
                    attempt, newInOnlyME.getExchangeId(),
                    newInOnlyME.getRole().toString(),
                    newInOnlyME.getStatus().toString());
        }

        send(newInOnlyME);
	}

    private void notifyResult(MessageExchange me, Result result) {
        JobExecutionContext jobExecContext = (JobExecutionContext)
                getContext().getCorrelationMap().remove(me.getExchangeId());
        if (jobExecContext != null) {
            synchronized (jobExecContext) {
                jobExecContext.setResult(result);
                jobExecContext.notifyAll();
            }
        }
    }
    
    public static class Result {

        private RedeliveryStatus redeliveryStatus;
        private Throwable error;

        public Result() {
            super();
        }

        public Result(Throwable error) {
            this();
            this.error = error;
        }
        
        public Result(RedeliveryStatus redeliveryStatus) {
            this();
            this.redeliveryStatus = redeliveryStatus;
            if (redeliveryStatus != null) {
                error = redeliveryStatus.getError();
            }
        }

        public RedeliveryStatus getRedeliveryStatus() {
            return redeliveryStatus;
        }

        public boolean hasFailed() {
            return (error != null) || ((redeliveryStatus != null)
                    && redeliveryStatus.hasFailed());
        }

        public Throwable getError() {
            return (error != null) ? error : (redeliveryStatus != null)
                    ? redeliveryStatus.getError() : null;
        }
    }
}
