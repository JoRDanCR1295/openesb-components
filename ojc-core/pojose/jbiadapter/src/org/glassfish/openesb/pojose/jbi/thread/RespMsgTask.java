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
 * @(#)RespMsgTask.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.jbi.thread;

import com.sun.jbi.common.util.NDC;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import org.glassfish.openesb.pojose.api.FaultMessage;
import org.glassfish.openesb.pojose.jbi.I18n;
import org.glassfish.openesb.pojose.jbi.POJOComponentContext;
import org.glassfish.openesb.pojose.jbi.nmr.ExecutorFactory;
import org.glassfish.openesb.pojose.jbi.nmr.POJOExecutor;
import org.glassfish.openesb.pojose.jbi.nmr.POJOExecutor.POJOExecutorMethods;
import org.glassfish.openesb.pojose.jbi.su.DummyProviderTracker;
import org.glassfish.openesb.pojose.jbi.su.MasterTracker;
import org.glassfish.openesb.pojose.jbi.su.PojoSEServiceUnit;
import org.glassfish.openesb.pojose.jbi.su.ProviderTracker;

/**
 * Runnable to process Response ME, i.e POJO SE is Consumer.
 * 
 * @author gpatil
 */
public class RespMsgTask extends BaseTask {
    private POJOComponentContext ctx;
    private MessageExchange consMe;
    private MessageExchange provMe;
    private ProviderTracker pt;
    
    private static final Logger logger = Logger.getLogger(
            org.glassfish.openesb.pojose.jbi.nmr.BasePojoExecutor.class.getName()); //PojoSE Executor

    /**
     * @param cc
     * @param me
     */
    public RespMsgTask(POJOComponentContext cc, MessageExchange me) {
        this.ctx = cc;
        this.consMe = me;
        this.pt = MasterTracker.getInstance().getProviderTrackerForConsumerME(consMe);
        this.provMe = pt.getProvisioningMsgEx();
        respMsgTaskCreated.incrementAndGet();
    }

    public String getPojoServiceClassName(){
        if (this.pt instanceof DummyProviderTracker){
            return ""; //NOI18N
        } else {
            return this.pt.getPOJOMetadata().getPojoClass().getName();
        }
    }

    public MessageExchange getProvisioningME(){
        if (this.pt instanceof DummyProviderTracker){
            return null;
        } else {
            return this.pt.getProvisioningMsgEx();
        }
    }

    public MessageExchange getTaskTriggerME(){
        return this.consMe;
    }
    
    public void run() {
        try {
            if (consMe != null){
                ServiceEndpoint consSept = consMe.getEndpoint();
                if (logger.isLoggable(Level.FINE)) {
                    String msg = I18n.lf("POJOSE-1510: Processing for consuming endpoint:{0}", consSept);//NOI18N
                    logger.fine(msg);
                }

                if (!this.pt.isProvisioningExchangeActive()){
                    if ((ExchangeStatus.DONE.equals(consMe.getStatus()))
                            || (ExchangeStatus.ERROR.equals(consMe.getStatus()))){
                        if (logger.isLoggable(Level.FINE)) {
                            String msg = I18n.lf("POJOSE-3500: Got {0} status from Endpoint:{1}", consMe.getStatus(),consSept);
                            logger.fine(msg);
                        }
                        String msg = I18n.loc("POJOSE-6500: Provisioning service ended in error before the response message from consumed service {0}, received.", consSept);
                        logger.warning(msg);
                    } else {
                        if (logger.isLoggable(Level.FINE)) {
                            String msg = I18n.lf("POJOSE-3500: Got {0} status from Endpoint:{1}", consMe.getStatus(),consSept);
                            logger.fine(msg);
                        }

                        String msg = I18n.loc("POJOSE-6500: Provisioning service ended in error before the response message from consumed service {0}, received.", consSept);
                        logger.warning(msg);
                        try {
                            consMe.setError(this.pt.getException());
                            ctx.getDC().send(consMe);
                        } catch (Throwable the){
                            msg = I18n.loc("POJOSE-7514: Got exception while sending error MessageExchange. {0}", the);
                            logger.severe(msg);
                        }
                    }

                    return;
                }

                if (ExchangeStatus.DONE.equals(consMe.getStatus())){
                    if (logger.isLoggable(Level.FINE)) {
                        String msg = I18n.lf("POJOSE-1511: Got DONE ME status for Endpoint:{0}", consSept);//NOI18N
                        logger.fine(msg);
                    }
                    pt.setReceivedDone(consMe);

                    if (pt.canExecuteOnDone()){
                        if (pt.tryAcquireLockForOnDoneExec()){
                            try {
                                execute(POJOExecutorMethods.onDone);
                            } finally {
                                pt.releaseLockForOnDoneExec();
                            }
                        }
                    }

                    return ;

                }else if (ExchangeStatus.ERROR.equals(consMe.getStatus())){
                    String msg = I18n.loc("POJOSE-6503: Got ERROR ME status from consuming Endpoint:{0}", consSept);//NOI18N
                    logger.warning(msg);
                    execute(POJOExecutorMethods.onError);
                    return ;
                } else {
                    Fault fault = consMe.getFault();
                    if (fault != null){
                        execute(POJOExecutorMethods.onFault);
                    }else{
                        execute(POJOExecutorMethods.onReply);
                    }
                }
            }
        } finally {
            respMsgTaskCompleted.incrementAndGet();
        }
    }

    private void execute(POJOExecutorMethods m){
        Exception exp = null;
        NDC ndc = null;

        Object pojoObj = this.pt.getPOJOInstance();
        Class pc = this.pt.getPOJOMetadata().getPojoClass();
        PojoSEServiceUnit su = ctx.getPojoSU(this.provMe.getEndpoint());

        POJOExecutor pojoExecutor = ExecutorFactory.getExecutor(this.ctx,
                this.pt.getPOJOMetadata(), pojoObj);
        
        try {
            ndc = NDC.enter("ServiceUnit", su.getName(), "POJO Provider Class", pc);
            if (POJOExecutorMethods.onDone.equals(m)){
                pojoExecutor.executeOnDone(provMe);
            }else if (POJOExecutorMethods.onReply.equals(m)){
                pojoExecutor.executeOnReply(provMe, consMe);
            }else if (POJOExecutorMethods.onError.equals(m)){
                pojoExecutor.executeOnError(provMe, consMe);
            }else if (POJOExecutorMethods.onFault.equals(m)){
                pojoExecutor.executeOnFault(provMe, consMe);
            }
            
        } catch (Throwable the){
            if (!(the instanceof Exception)){
                exp = new Exception(the);
            }
        } finally {
            if (ndc != null){
                ndc.exit();
            }
        }

        if (exp != null){
            try {
                if (exp instanceof FaultMessage){
                    FaultMessage fm = (FaultMessage) exp;
                    this.provMe.setFault(fm.getFault());
                } else {
                    this.provMe.setError(exp);
                }
                ctx.getDC().send(this.provMe);
                this.pt.setDoneWithProvisioning();
            } catch (MessagingException ex) {
                Logger.getLogger(InboundProcessor.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }

    @Override
    public String toString() {
        return "RespMsgTask:" + ( (this.provMe != null) ? this.provMe.getExchangeId(): "provMe==null") + ":"+ this.consMe.getExchangeId(); //NOI18N
    }
}
