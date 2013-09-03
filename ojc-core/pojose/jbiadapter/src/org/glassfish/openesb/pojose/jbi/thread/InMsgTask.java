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
 * @(#)InMETask.java
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
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import org.glassfish.openesb.pojose.core.anno.meta.OperationMetadata;
import org.glassfish.openesb.pojose.core.anno.meta.POJOClassMetadata;
import org.glassfish.openesb.pojose.core.anno.processor.POJOAnnotationProcessor;
import org.glassfish.openesb.pojose.core.util.TransactionHelper;
import org.glassfish.openesb.pojose.core.util.Util;
import org.glassfish.openesb.pojose.jbi.I18n;
import org.glassfish.openesb.pojose.jbi.POJOComponentContext;
import org.glassfish.openesb.pojose.jbi.nmr.ExecutorFactory;
import org.glassfish.openesb.pojose.jbi.nmr.POJOExecutor;
import org.glassfish.openesb.pojose.jbi.su.MasterTracker;
import org.glassfish.openesb.pojose.jbi.su.PojoSEServiceUnit;
import org.glassfish.openesb.pojose.jbi.su.ProviderTracker;
import org.glassfish.openesb.pojose.res.impl.CallTracker;
import org.glassfish.openesb.pojose.res.impl.ContextImpl;
import org.glassfish.openesb.pojose.res.impl.POJOContextImpl;

/**
 * Runnable to process Input ME, i.e POJO SE is Provider.
 *
 * Handles Done, Error MEs. Checks for ME pattern inconsistencies.
 * Instantiates POJO using appropriate CL and Txn context (for synch mode only).
 * Delegates execution of Operation and OnDone methods and updating ProviderTracker
 * to Executors.
 * 
 * @author gpatil
 */
public class InMsgTask extends BaseTask {
    private POJOComponentContext ctx;
    private MessageExchange me;
    private ProviderTracker pt;
    private static final Logger logger = Logger.getLogger(
            org.glassfish.openesb.pojose.jbi.nmr.BasePojoExecutor.class.getName()); //PojoSE Executor

    /**
     * @param cc
     * @param me
     */
    public InMsgTask(POJOComponentContext cc, MessageExchange me) {
        this.ctx = cc;
        this.me = me;
        this.pt = MasterTracker.getInstance().getProviderTracker(me);
        init();
        inMsgTaskCreated.incrementAndGet();
    }

    private void init(){
        ServiceEndpoint sept = me.getEndpoint();
        PojoSEServiceUnit su = ctx.getPojoSU(sept);
        POJOClassMetadata pojoMeta = su.getPojoClassMetadata(sept);
        this.pt.setPOJOMetadata(pojoMeta);
    }

    public String getPojoServiceClassName(){
        return pt.getPOJOMetadata().getPojoClass().getName();
    }

    public MessageExchange getProvisioningME(){
        return this.me;
    }

    public MessageExchange getTaskTriggerME(){
        return this.me;
    }
    
    public void run() {
        try{
            ServiceEndpoint sept = me.getEndpoint();
            String msg = null;

            if (ExchangeStatus.DONE.equals(me.getStatus())){
                if (pt != null){
                    pt.setDoneWithProvisioning();
                } else {
                    msg = I18n.loc("POJOSE-7515: Internal Error. ProviderTracker not found for Endpoint {0}.", sept);
                    logger.severe(msg);
                }

                if (logger.isLoggable(Level.FINE)) {
                    msg = I18n.lf("POJOSE-1511: Got DONE ME status for Endpoint:{0}", sept);//NOI18N
                    logger.fine(msg);
                }
                // No need to commit TXN, since, POJO SE is not the TXN initiating component.
                // POJO SE just participates in, propagates TXN.
                // TODO Handle done. update stats.
                return ;
            } else if (ExchangeStatus.ERROR.equals(me.getStatus())){
                if (pt != null){
                    pt.setDoneWithProvisioning();
                } else {
                    msg = I18n.loc("POJOSE-7515: Internal Error. ProviderTracker not found for Endpoint {0}.", sept);
                    logger.severe(msg);
                }

                msg = I18n.loc("POJOSE-7511: Got ERROR ME status for Endpoint:{0}", sept);
                logger.severe(msg);
                // No need to rollback TXN, since, POJO SE is not the initiating component.
                //TODO Handle error calling @OnProvisioningError method. update stats.
                return ;
            }

            executePojoOperation(ctx, sept, me);
        } finally {
            inMsgTaskCompleted.incrementAndGet();
        }
    }

    /**
     * Handling "In" Message.
     * 
     * @param ctx
     * @param sept
     * @param me
     */
    private void executePojoOperation(POJOComponentContext ctx,
            ServiceEndpoint sept, MessageExchange me){
        
        boolean executed = false;
        Exception exp = null;

        try {
            PojoSEServiceUnit su = ctx.getPojoSU(sept);
            POJOClassMetadata pojoMeta = this.pt.getPOJOMetadata();

            if (pojoMeta != null){
                NDC ndc = null;

                // TODO refactor/move logic
                if ((pojoMeta.getMEPStyle() == Util.MEPStyle.InOnly) &&
                    (!(me instanceof InOnly))){
                    String msg = I18n.loc("POJOSE-7512: Did not get the expected InOnly ME, got {0}", me.getPattern());
                    logger.severe(msg);
                    Exception ex = new Exception (msg);
                    me.setError(ex);
                    ctx.getDC().send(me);
                    executed = true;

                    this.pt.setException(ex);
                    this.pt.setDoneWithProvisioning();

                    return;
                }

                if ((pojoMeta.getMEPStyle() == Util.MEPStyle.InOut) &&
                    (!(me instanceof InOut))){
                    String msg = I18n.loc("POJOSE-7513: Did not get the expected InOut ME, got {0}", me.getPattern());
                    logger.severe(msg);
                    Exception ex = new Exception (msg);
                    me.setError(ex);
                    ctx.getDC().send(me);
                    executed = true;

                    this.pt.setException(ex);
                    this.pt.setDoneWithProvisioning();

                    return;
                }

                try {
                    Class pc = pojoMeta.getPojoClass();
                    ndc = NDC.enter("ServiceUnit", su.getName(), "POJO Provider Class", pc);

                    ClassLoader thdCtxCl = Thread.currentThread().getContextClassLoader();
                    Object pojoObj = null;
                    try {
                        // Ctx CL
                        Thread.currentThread().setContextClassLoader(pc.getClassLoader());

                        // Ctx Txn
                        if (me.isTransacted()){
                            TransactionHelper.resumeTransaction(ctx.getJBIComponentContext().getTransactionManager(),
                                    me);
                        }

                        pojoObj = pc.newInstance();

                        this.pt.setPOJOInstance(pojoObj);
                        
                    } catch (Exception ex) {
                        exp = ex;
                        this.pt.setException(exp);
                        Logger.getLogger(InMsgTask.class.getName()).log(Level.SEVERE, null, ex);
                    } finally {
                        // Ctx CL
                        if (thdCtxCl != null){
                            Thread.currentThread().setContextClassLoader(thdCtxCl);
                        }

                        // Ctx Txn
                        if (me.isTransacted()){
                            TransactionHelper.suspendTransaction(ctx.getJBIComponentContext().getTransactionManager(), me);
                        }
                    }

                    if (exp == null){
                        if (pojoMeta.getProvider() != null){
                            // Using current @Provider annotation.
                            ContextImpl ctximpl = new ContextImpl(ctx.getJBIComponentContext(), ctx.getDC(), (CallTracker)this.pt, ctx.getConfigMbean().retrieveApplicationVariablesMap());
                            ctximpl.setMessageExchange(me);

                            POJOAnnotationProcessor.injectFields(pojoMeta, pojoObj, ctximpl, (CallTracker)this.pt);
                        } else {
                            // Using old @POJO annotation.
                            ContextImpl ctximpl = new ContextImpl(ctx.getJBIComponentContext(), ctx.getDC(), (CallTracker)this.pt, ctx.getConfigMbean().retrieveApplicationVariablesMap());
                            ctximpl.setMessageExchange(me);

                            POJOContextImpl pjctx = new POJOContextImpl(ctx.getJBIComponentContext(), ctx.getDC());
                            pjctx.setMessageExchange(me);

                            POJOAnnotationProcessor.injectFields(pojoMeta, ctximpl, pjctx, pojoObj);
                        }

                        OperationMetadata opm = pojoMeta.getOperationMetadata();

                        if (opm != null){
                            POJOExecutor pojoExecutor = ExecutorFactory.getExecutor(ctx,
                                    pojoMeta, pojoObj);
                            // Execuotr executes OnDone and updates ProviderTracker.
                            pojoExecutor.executePojoOperation(me);
                            executed = true;                            
                        } else {
                            String msg = I18n.loc("POJOSE-7519: Could not find appropriate method with Operation annotation in class {0}.", pojoMeta.getPojoClass().getName());
                            logger.severe(msg);
                            exp = new Exception(msg);
                        }
                    }
                } finally {
                    if (ndc != null){
                        ndc.exit();
                    }
                }
            } else {
                String msg = I18n.loc("POJOSE-7518: Internal error. POJO Metadata not found for endpoint {0}.", sept);
                logger.severe(msg);
                exp = new Exception(msg);
            }
        } catch (Exception ex) {
            Logger.getLogger(InboundProcessor.class.getName()).log(Level.SEVERE, null, ex);
            exp = ex;
        }

        if (!executed){
            try {
                executed = true;
                me.setError(exp);
                ctx.getDC().send(me);
                this.pt.setDoneWithProvisioning();
            } catch (MessagingException ex) {
                Logger.getLogger(InboundProcessor.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }

    @Override
    public String toString() {
        return "InMsgTask:" + this.me.getExchangeId(); //NOI18N
    }
}
