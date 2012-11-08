/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.glassfish.openesb.pojose.jbi.su;

import javax.jbi.messaging.MessageExchange;
import org.glassfish.openesb.pojose.core.anno.meta.POJOClassMetadata;
import org.glassfish.openesb.pojose.jbi.I18n;

/**
 *
 * @author gpatil
 */
public class DummyProviderTracker implements ProviderTracker{
    private static ProviderTracker instance = new DummyProviderTracker();
    private Exception ex;

    private DummyProviderTracker(){
    }

    public static ProviderTracker getInstance(){
        return instance;
    }

    public boolean isProvisioningExchangeActive() {
        return false;
    }

    public boolean canExecuteOnDone() {
        return false;
    }

    public Exception getException() {
        if (ex == null){
            String msg = I18n.loc("POJOSE-7516: POJO Service ended with exception before response message recieved.");
            this.ex = new Exception(msg);
        }
        return this.ex;
    }

    public Object getPOJOInstance() {
        throw new UnsupportedOperationException("Not supported yet."); //NOI18N
    }

    public POJOClassMetadata getPOJOMetadata() {
        throw new UnsupportedOperationException("Not supported yet."); //NOI18N
    }

    public MessageExchange getProvisioningMsgEx() {
        return null; //NOI18N
    }

    public boolean isOperationExecuted() {
        throw new UnsupportedOperationException("Not supported yet."); //NOI18N
    }

    public void releaseLockForOnDoneExec() {
        throw new UnsupportedOperationException("Not supported yet."); //NOI18N
    }

    public boolean tryAcquireLockForOnDoneExec() {
        throw new UnsupportedOperationException("Not supported yet."); //NOI18N
    }


    public void setDoneWithProvisioning() {
        throw new UnsupportedOperationException("Not supported yet."); //NOI18N
    }

    public void setException(Exception pojo) {
        throw new UnsupportedOperationException("Not supported yet."); //NOI18N
    }

    public void setExecutedOnError(MessageExchange consME) {
        throw new UnsupportedOperationException("Not supported yet."); //NOI18N
    }

    public void setExecutedOnFault(MessageExchange consME) {
        throw new UnsupportedOperationException("Not supported yet."); //NOI18N
    }

    public void setExecutedOnReply(MessageExchange consME) {
        throw new UnsupportedOperationException("Not supported yet."); //NOI18N
    }

    public void setExecutedOperation() {
        throw new UnsupportedOperationException("Not supported yet."); //NOI18N
    }

    public void setPOJOInstance(Object pojo) {
        throw new UnsupportedOperationException("Not supported yet."); //NOI18N
    }

    public void setPOJOMetadata(POJOClassMetadata meta) {
        throw new UnsupportedOperationException("Not supported yet."); //NOI18N
    }

    public void setReceivedDone(MessageExchange consME) {
        throw new UnsupportedOperationException("Not supported yet."); //NOI18N
    }

    public void setSentProviderResponse() {
        throw new UnsupportedOperationException("Not supported yet."); //NOI18N
    }

    public void setExecutedOnDone() {
        throw new UnsupportedOperationException("Not supported yet."); //NOI18N
    }
}
