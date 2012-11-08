package com.sun.jbi.common.qos.messaging;

import java.util.Iterator;
import java.util.Properties;
import java.util.Set;
import java.util.Map.Entry;

import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

import com.sun.jbi.common.qos.I18n;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.util.Util;

public class BaseExchangeTemplates implements ExchangeTemplates {
    private ServiceEndpoint mEndpoint; 
    private QName mOperation;
    private boolean mOneWay;
    private String mGroupId;
    private String mMsgId;
    private Source mInput;
    private Properties mPropExchange;
    private Properties mPropNM;
    private MessageExchangeFactory mExchangeFactory;
    private Transaction mTransaction;
    private TransactionManager mTxMgr;
    private boolean mRedelivery = true;

    private MessageExchange mex = null;
    
    public BaseExchangeTemplates(
            ServiceEndpoint endpoint, 
            QName operation,
            boolean oneWay,
            String groupId,
            String msgId,
            Source input,
            MessageExchangeFactory exchangeFactory ){
        this.mEndpoint = endpoint;
        this.mOperation = operation;
        this.mOneWay = oneWay;
        this.mGroupId = groupId;
        this.mMsgId = msgId;
        this.mInput = input;
        this.mExchangeFactory = exchangeFactory;
    }
    public MessageExchange createExchange() throws MessagingException {
        if(!mRedelivery){
            String msg = I18n.loc(
                            "MessageExchange with messageid {0}, groupid {1} can not participate in redelivery",
                            mMsgId, mGroupId);
            throw new MessagingException(msg);
        }
        //if it is not the first attempt, need to cleanup
        if (mex != null) {
            Transaction tmpTransaction = (Transaction) mex
                    .getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
            if (tmpTransaction != null) {
                try {
                    tmpTransaction.rollback();
                } catch (Exception e) {
                    String msg = I18n.loc("Not able to rollback transaction Redelivery is aborted for messageid {0}, groupid {1} can not participate in redelivery",
                                    mMsgId, mGroupId);
                    throw new MessagingException(msg, e);
                }
            }
        }
        //create MEx
        if (mOneWay) {
            mex = mExchangeFactory.createInOnlyExchange();
        } else {
            mex = mExchangeFactory.createInOutExchange();
        }
        //set com.sun.jbi.messaging.messageid
        mex.setProperty(ServiceQuality.MESSAGE_ID, mMsgId);
        
        //set com.sun.jbi.messaging.groupid
        if ((mGroupId != null) && (mGroupId.trim().length() > 0)) {
            mex.setProperty(ServiceQuality.GROUP_ID, mGroupId);
        }
        
        //set operation name
        mex.setOperation(mOperation);
        
        //set EP
        mex.setEndpoint(mEndpoint);
        
        //populate the Txn
        if (mTransaction != null) {
            mex.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME,
                    mTransaction);
            mRedelivery = false;
        } else if (mTxMgr!=null){
            Transaction tmpTransaction;
            try {
                mTxMgr.begin();
                tmpTransaction = mTxMgr.getTransaction();
                mTxMgr.suspend();
            } catch (Exception e) {
                String msg = I18n.loc(
                        "Not able create/associate transaction Redelivery is aborted for messageid {0}, groupid {1} can not participate in redelivery",
                        mMsgId, mGroupId);
                throw new MessagingException(msg, e);
            }
            mex.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME,
                    tmpTransaction);
        }

        //populate MEx properties if they are set
        if (mPropExchange != null) {
            Set<Entry<Object, Object>> set = mPropExchange.entrySet();
            Iterator<Entry<Object, Object>> itr = set.iterator();
            while (itr.hasNext()) {
                Entry<Object, Object> entry = itr.next();
                mex.setProperty((String) entry.getKey(), entry.getValue());
            }
        }
        

        //set input
        NormalizedMessage nmsg = mex.createMessage();
        nmsg.setContent(mInput);
        if (mOneWay) {
            ((InOnly)mex).setInMessage(nmsg);
        } else {
            ((InOut)mex).setInMessage(nmsg);
        }
        //populate N MSG properties if they are set
        if (mPropNM != null) {
            Set<Entry<Object, Object>> setNM = mPropNM.entrySet();
            Iterator<Entry<Object, Object>> itrNM = setNM.iterator();
            while (itrNM.hasNext()) {
                Entry<Object, Object> entryNM = itrNM.next();
                nmsg.setProperty((String) entryNM.getKey(), entryNM.getValue());
            }
        }
        
        return mex;
    }
    public void setPropExchange(Properties propExchange) {
        this.mPropExchange = propExchange;
    }
    public void setPropNM(Properties propNM) {
        this.mPropNM = propNM;
    }
    public void setTransaction(Transaction transaction) {
        this.mTransaction = transaction;
    }
    public void setTransactionManager(TransactionManager tm) {
        this.mTxMgr = tm;
    }
    
    public String getUniqueId() {
        if (Util.isEmpty(mMsgId)) return null;
        else {
            return (Util.isEmpty(mGroupId)) ? mMsgId : mMsgId +"."+ mGroupId; 
        }
    }

}
