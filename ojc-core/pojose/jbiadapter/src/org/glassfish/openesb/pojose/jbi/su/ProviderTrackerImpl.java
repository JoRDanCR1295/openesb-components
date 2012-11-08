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
 * @(#)ProviderTracker.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.jbi.su;

import com.sun.jbi.common.qos.ServiceQuality;
import java.util.List;
import java.util.Vector;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Logger;
import javax.jbi.messaging.MessageExchange;
import org.glassfish.openesb.pojose.core.anno.meta.POJOClassMetadata;
import org.glassfish.openesb.pojose.jbi.I18n;
import org.glassfish.openesb.pojose.jbi.thread.BaseTask;
import org.glassfish.openesb.pojose.jbi.thread.RespMsgTask;
import org.glassfish.openesb.pojose.jbi.thread.TaskThread;
import org.glassfish.openesb.pojose.res.impl.CallTracker;

/**
 *
 * @author gpatil
 */
public class ProviderTrackerImpl implements ProviderTracker, CallTracker{
    private volatile boolean opExecuted = false;
    private volatile boolean doneWithProvisioning = false;
    private volatile boolean sentProvResponse = false;
    private volatile boolean onDoneExecuted = false;

    // TODO Do we really need volatile on Objects refences?
    private volatile Exception msgException;
    private volatile Object pojoInstance;
    private volatile POJOClassMetadata pojoMeta;
    
    private volatile AtomicInteger outStandingASynchCalls = new AtomicInteger(0);
    private volatile AtomicInteger uniqueMsgIdCtr = new AtomicInteger(0);
    private volatile Semaphore onDoneLock = new Semaphore(1);

    private final List<String> consMsgExs = new Vector<String>();
    private final MasterTracker mt;
    private final MessageExchange providerME;

    private final ReentrantLock txnPropagationFalgLock = new ReentrantLock();
    private String txnPropagatedMsgExId = null;

    private final Logger logger = Logger.getLogger("org.glassfish.openesb.pojose.jbi.nmr.BasePojoExecutor"); //NOI18N
    
    ProviderTrackerImpl(MasterTracker mt, MessageExchange provME){
        this.mt = mt;
        this.providerME = provME;
    }

    // ***** Start of ASynchCallTracker methods  ****
    public void updateActiveSynchCalls(boolean increment){
        this.mt.updateActiveSynchCalls(increment);
    }

    public void sentAsynchInMsg(MessageExchange provME, MessageExchange consME){
        this.mt.addConsMsgEx(provME, consME);
        this.consMsgExs.add(MasterTracker.getConsMexId(consME));
        outStandingASynchCalls.incrementAndGet();
    }

    public void setSendASynchInMsgError(MessageExchange consME) {
        outStandingASynchCalls.decrementAndGet();
        this.consMsgExs.remove(MasterTracker.getConsMexId(consME));
        this.mt.removeConsMsgEx(this.providerME, consME);
    }
    
    public boolean isValid2CallASynchInOnly(){
        boolean ret = false;
        if (this.pojoMeta != null){
            ret = this.pojoMeta.isValidToCallASynchInOnly();
        }

        return ret;
    }

    public boolean isValid2CallASynchInOut(){
        boolean ret = false;
        if (this.pojoMeta != null){
            ret = this.pojoMeta.isValidToCallASynchInOut();
        }

        return ret;
    }

    public boolean isValid2CallASynch() {
        boolean ret = false;
        if (this.pojoMeta != null){
            ret = this.pojoMeta.isAsynchConsumer();
        }
        return ret;

    }

    public boolean isExecutingOnDone(){
        boolean ret = false;
        if (this.onDoneLock.availablePermits() <= 0){
            ret = true;
        }
        return ret;
    }

    public String getUniqueMsgId() {
        return this.providerME.getExchangeId() + ":" + this.uniqueMsgIdCtr.incrementAndGet();//NOI18N
    }

//    public boolean isOkToPropagateTxn() {
//        try {
//            this.txnPropagationFalgLock.lock();
//            if (this.txnPropagatedMsgExId != null){
//                String m = I18n.loc("POJOSE-6504: Can not propagate transaction, as it is progated with asynchronously with MessageExchange {0}={1}.", ServiceQuality.MESSAGE_ID, this.txnPropagatedMsgExId);
//                logger.warning(m);
//                return false;
//            } else {
//                return true;
//            }
//        } finally {
//            this.txnPropagationFalgLock.unlock();
//        }
//    }

    // ***Mutable*** operation! 
    public boolean isOkToPropagateTxn(MessageExchange consME) {
        try {
            this.txnPropagationFalgLock.lock();
            if (this.txnPropagatedMsgExId != null){
                if (Thread.currentThread() instanceof TaskThread){
                    TaskThread tt = (TaskThread) Thread.currentThread();
                    BaseTask bt = tt.getTarget();
                    if (bt instanceof RespMsgTask){
                        RespMsgTask rmt = (RespMsgTask) bt;
                        MessageExchange triggerConsME = rmt.getTaskTriggerME();
                        String id = MasterTracker.getConsMexId(triggerConsME);
                        if (this.txnPropagatedMsgExId.equals(id)){
                            this.txnPropagatedMsgExId = id;
                            return true;
                        }
                    }
                }

                return false;
            } else {
                this.txnPropagatedMsgExId = MasterTracker.getConsMexId(consME);
                return true;
            }
        } finally {
            this.txnPropagationFalgLock.unlock();
        }
    }

    public synchronized boolean isOkToResumeTxn(MessageExchange consME){
        try {
            this.txnPropagationFalgLock.lock();
            if ((this.txnPropagatedMsgExId != null) &&
                    (this.txnPropagatedMsgExId.equals(MasterTracker.getConsMexId(consME)))){
                return true;
            } else {
                return false;
            }
        } finally {
            this.txnPropagationFalgLock.unlock();
        }
    }

    // ***** End of ASynchCallTracker methods  ****

    // ***** Start of Impl for ProviderTracker *****

    public MessageExchange getProvisioningMsgEx() {
        return this.providerME;
    }

    public boolean isProvisioningExchangeActive() {
        return true;
    }


    public boolean canExecuteOnDone(){
        long i = this.outStandingASynchCalls.get();
        boolean ret = (this.opExecuted && (i <= 0));
        //##### Remove Me
        //Exception ex = new Exception("canExecuteOnDone:" + ret + ": " + i);
        //ex.printStackTrace();
        //##### Remove Me end
        return ret;
    }

    public boolean isOperationExecuted() {
        return this.opExecuted;
    }

    public void releaseLockForOnDoneExec() {
        this.onDoneLock.release();
    }

    public synchronized boolean tryAcquireLockForOnDoneExec() {
        boolean ret = false;
        if (!this.onDoneExecuted){
            ret = this.onDoneLock.tryAcquire();
        }
        return ret;
    }

    public void setDoneWithProvisioning() {
        this.doneWithProvisioning = true;
        //##### TODO Remove ME
        //Exception ex = new Exception("Done with Provisioning is called!!!");
        //ex.printStackTrace();
        //##### TODO Remove ME - End
        this.mt.removeProviderTracker(this.providerME, consMsgExs);
        //##### TODO comment-out below line after debugging/testing.
        //printStats();
    }

    public void setExecutedOperation(){
        this.opExecuted = true;
    }

    public void setSentProviderResponse() {
        this.sentProvResponse = true;
    }

    public void setReceivedDone(MessageExchange consME) {
        outStandingASynchCalls.decrementAndGet();
        this.consMsgExs.remove(MasterTracker.getConsMexId(consME));
        this.mt.removeConsMsgEx(this.providerME, consME);
    }

    public void setExecutedOnReply(MessageExchange consME) {
        outStandingASynchCalls.decrementAndGet();
        this.consMsgExs.remove(MasterTracker.getConsMexId(consME));
        this.mt.removeConsMsgEx(this.providerME, consME);
    }

    public void setExecutedOnFault(MessageExchange consME) {
        outStandingASynchCalls.decrementAndGet();
        this.consMsgExs.remove(MasterTracker.getConsMexId(consME));
        this.mt.removeConsMsgEx(this.providerME, consME);
    }

    public void setExecutedOnError(MessageExchange consME) {
        outStandingASynchCalls.decrementAndGet();
        this.consMsgExs.remove(MasterTracker.getConsMexId(consME));
        this.mt.removeConsMsgEx(this.providerME, consME);
    }

    public void setExecutedOnDone() {
        this.onDoneExecuted = true;
    }

    public Exception getException() {
        return this.msgException;
    }

    public Object getPOJOInstance() {
        return this.pojoInstance;
    }

    public void setPOJOInstance(Object pojo) {
        this.pojoInstance = pojo;
    }

    public void setException(Exception ex){
        this.msgException = ex;
    }

    public POJOClassMetadata getPOJOMetadata() {
        return this.pojoMeta;
    }

    public void setPOJOMetadata(POJOClassMetadata meta) {
        this.pojoMeta = meta;
    }


    // ***** End of Impl for ProviderTracker *****

    private void printStats(){
        logger.info("outStandingASynchCalls:" + this.outStandingASynchCalls.get());
        logger.info("Cons MEXs:" + this.consMsgExs.size());
        logger.info("opExecuted:" + this.opExecuted);
        logger.info("doneWithProvisioning:" + this.doneWithProvisioning);
        logger.info("sentProvResponse:" + this.sentProvResponse);
        logger.info("MsgExp:" + this.msgException);
        logger.info("POJO Instance:" + this.pojoInstance);
    }
}
