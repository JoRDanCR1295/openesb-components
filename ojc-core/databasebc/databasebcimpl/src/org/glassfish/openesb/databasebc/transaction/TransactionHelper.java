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
 * @(#)TransactionHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.transaction;

import com.sun.jbi.internationalization.Messages;
import javax.transaction.Status;
import javax.transaction.Synchronization;
import javax.transaction.SystemException;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;        
import javax.transaction.RollbackException;

import java.util.ArrayList;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;

import javax.sql.XAConnection;
import javax.sql.XADataSource;
import java.sql.SQLException;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.transaction.NotSupportedException;
import javax.transaction.HeuristicMixedException;
import javax.transaction.HeuristicRollbackException;
import javax.transaction.InvalidTransactionException;
import javax.transaction.xa.Xid;

/**
 *
 * @author Narayanaa
 */
public class TransactionHelper {
    
    //private static final Messages mMessages = Messages.getMessages(TransactionHelper.class);

    private static final Logger mLogger = Messages.getLogger(TransactionHelper.class);

    
    /** Creates a new instance of TransactionHelper */
    public TransactionHelper() {
        xid = xidFactory.newXid();
        gid = xid.getTrulyGlobalId();
        status = Status.STATUS_ACTIVE;
        start = System.currentTimeMillis();
    }
    
    public void setComponentContext(ComponentContext componentContext) throws Exception{
        manager = (TransactionManager)componentContext.getTransactionManager();
		transaction = manager.getTransaction();
		if(transaction == null){
			begin(); 
			transaction = manager.getTransaction();
		}
        status = manager.getStatus();
        xid = xidFactory.newXid();
    }
    
    public void setMessageExchange(MessageExchange messageExchange) throws Exception{
        this.messageExchange = messageExchange;
    }
    
    public boolean isInOnly(MessageExchange me) {
        return (me instanceof InOnly);
    }
    
    public boolean isTxEnabled(MessageExchange me) { 
		return ((tx_enable==null || tx_enable.equalsIgnoreCase("true")) 
			&& !isInOnly(me)); 
    }
    
    public void setXAResource(XAResource xar){
        this.xar = xar;
        start = System.currentTimeMillis();
        try{
            this.timeoutPeriod = xar.getTransactionTimeout();
        }catch(XAException xae){
            mLogger.log(Level.SEVERE,"Error While setting the XA Resource",xae);
        }
    }
    
    public void setDataSource(XADataSource xads){
        this.xads = xads;
        try{
            xac = xads.getXAConnection();
            xar = xads.getXAConnection().getXAResource();
            start = System.currentTimeMillis();
            this.timeoutPeriod = xar.getTransactionTimeout();
        }catch(SQLException se){
            mLogger.log(Level.SEVERE,"Error While setting the Data Source",se);
            //se.getStackTrace();
        }catch(XAException xe){
            mLogger.log(Level.SEVERE,"Error While getting the XA Connection",xe);
            //xe.getStackTrace();
        }
    }

    public void registerXAResouce()	{	

//		componentContext = (com.sun.jbi.framework.ComponentContext)componentContext;
//		componentContext.registerXAResource(xar);
	}

    public XAConnection	getXAConnection(){
		return this.xac;
	}
    
    public void handleInbound(MessageExchange me) throws Exception {
 	        if(!isTxEnabled(me))
 	            return;
 	        suspendTx(me);
     }
 	
    public void handleOutbound(MessageExchange me) throws Exception {
 	        if(!isTxEnabled(me))
 	            return;
 	        resumeTx(me);
    }
    
    public TransactionManager getTransactionManager (){
        return this.manager;
    } 
   
    public void suspendTx(MessageExchange me) throws SystemException {
        TransactionManager tm = getTransactionManager();
        Transaction tx = tm.getTransaction();
        if (tx != null) {
            if(me.getStatus().equals(ExchangeStatus.ERROR))
                tx.setRollbackOnly();
            tx = tm.suspend();
            me.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, tx);
        }
    }
 	
    public void resumeTx(MessageExchange me) throws
 	            SystemException,
 	            InvalidTransactionException {
         TransactionManager tm = getTransactionManager();
         Transaction tx = (Transaction)me.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
        if(tx != null) {
            if(me.getStatus().equals(ExchangeStatus.ERROR))
                tx.setRollbackOnly();
            manager.resume(tx);
        }
    }
     
    //@Override
    public int hashCode() {
        return xid.hashCode();
    }
    
    //@Override
    public String toString() {
        return "TransactionHelper:" + xidFactory.toString(xid);
    }
    
    //@Override
    public boolean equals(Object obj) {
        if (obj != null && obj instanceof TransactionHelper)
            return getLocalIdValue() == (((TransactionHelper)obj).getLocalIdValue());
        return false;
    }
    
    
    /**
     *  Returns the local id of this transaction. The local id is used as
     *  a transaction propagation context within the JBoss server, and
     *  in the TxManager for mapping local transaction ids to transactions.
     */
    public long getLocalIdValue() {
        return xid.getLocalIdValue();
    }
    
    /**
     *  Returns the local id of this transaction. The local id is used as
     *  a transaction propagation context within the JBoss server, and
     *  in the TxManager for mapping local transaction ids to transactions.
     */
    public LocalId getLocalId() {
        return xid.getLocalId();
    }
    
    /**
     *  Returns the global id of this transaction. Ths global id is used in
     *  the TxManager, which keeps a map from global ids to transactions.
     */
    public GlobalId getGlobalId() {
        return gid;
    }
    
    /**
     *  Returns the xid of this transaction.
     */
    public XidImpl getXid() {
        return xid;
    }

    private long lastBranchId = 0;
    private Xid createXidBranch() {
        long branchId = ++lastBranchId;
        return xidFactory.newBranch(xid, branchId);
    }
    
   // commit the transaction underconsideration
   // 
    public void commit()
        throws RollbackException,                
                java.lang.SecurityException,
                java.lang.IllegalStateException,
                SystemException,
                Exception{
        status = transaction.getStatus();
        if(lock.tryLock()){
            try {
                
                if (status == Status.STATUS_COMMITTED){
                    xar.commit(xid,true);
                    transaction.commit();
               //     checkHeuristics();
                }else{
                    Throwable causedByThrowable = exceptionReason;
                    //rollbackResources();
                    //completeTransaction();
                    xar.rollback(xid);
                    transaction.rollback();
                    // throw jboss rollback exception with the saved off exceptionReason
                    mLogger.log(Level.SEVERE,"Error While Commiting the transaction",causedByThrowable);
                    throw new Exception("Unable to commit, tx=" +
                            toString() + " status=" + TransactionStateUtility.getStringStatus(status),
                            causedByThrowable);
                }
            } finally {
                lock.unlock();
            }
        }
    }
    
    public void rollback()
        throws java.lang.IllegalStateException,
                java.lang.SecurityException,
                SystemException {
        status = transaction.getStatus();
        if(lock.tryLock()){
            try {
                
                switch (status) {
                    case Status.STATUS_ACTIVE:
                        status = Status.STATUS_MARKED_ROLLBACK;
                        transaction.setRollbackOnly();
                    case Status.STATUS_MARKED_ROLLBACK:
                        //endResources();
                        xar.end(xid,XAResource.TMFAIL);
                        transaction.rollback();
                    case Status.STATUS_PREPARING:
                        status = Status.STATUS_MARKED_ROLLBACK;
                        transaction.setRollbackOnly();
                        return; 
                    default:
                        mLogger.log(Level.SEVERE,"Error While Setting the Transaction Status");
                        throw new IllegalStateException("Cannot rollback(), " +
                                "tx=" + toString() +
                                " status=" +
                                TransactionStateUtility.getStringStatus(status));
                }
            } catch(XAException xe){
                mLogger.log(Level.SEVERE,"Error While Setting the Transaction Status",xe);
                //xe.getStackTrace();
            }finally {
                //Thread.interrupted();// clear timeout that did an interrupt
                lock.unlock();
            }
        }
    }
    
    public boolean enlist() throws RollbackException,
            java.lang.IllegalStateException,
            SystemException {
       
        if (xar == null)
            throw new IllegalArgumentException("null xaRes tx=" + this);
        
        //get status from the TM
		if(transaction != null)
	        status = transaction.getStatus();
        
        if(lock.tryLock()){
            try {
                switch (status) {
                    case Status.STATUS_ACTIVE:
                    case Status.STATUS_PREPARING:
                        break;
                    case Status.STATUS_PREPARED:
                        throw new IllegalStateException("Already prepared. " + this);
                    case Status.STATUS_COMMITTING:
                        throw new IllegalStateException("Already started committing. " + this);
                    case Status.STATUS_COMMITTED:
                        throw new IllegalStateException("Already committed. " + this);
                    case Status.STATUS_MARKED_ROLLBACK:
                        throw new RollbackException("Already marked for rollback " + this);
                    case Status.STATUS_ROLLING_BACK:
                        throw new RollbackException("Already started rolling back. " + this);
                    case Status.STATUS_ROLLEDBACK:
                        throw new RollbackException("Already rolled back. " + this);
                    case Status.STATUS_NO_TRANSACTION:
						if(transaction == null)
							break;
						else
		                    throw new IllegalStateException("No transaction. " + this);
                    case Status.STATUS_UNKNOWN:
                        throw new IllegalStateException("Unknown state " + this);
                    default:
                        throw new IllegalStateException("Illegal status: " + TransactionStateUtility.getStringStatus(status) + " tx=" + this);
                }
                isEnlisted = transaction.enlistResource(xar);
            } finally {
                lock.unlock();
            }
        }
        return isEnlisted;
    }
    
    public void delist()throws java.lang.IllegalStateException,
                                     SystemException,XAException {
        if(lock.tryLock()){
            try{
                delist(transaction.getStatus());
            }finally{
                lock.unlock();
            }
        }
    }
    
    public void begin() throws NotSupportedException,SystemException{
        if(lock.tryLock()){
            try{
                manager.begin();
            }finally{
                lock.unlock();
            }
        }
    }
    
    public void TMCommit() throws RollbackException,
                            HeuristicMixedException,
                            HeuristicRollbackException,
                            SystemException{
        if(lock.tryLock()){
            try{
                manager.commit();
            }catch(RollbackException re){
                try{
                    manager.rollback();
                }catch(SystemException se){
                    mLogger.log(Level.SEVERE,"Error While Commiting the Transaction",se);
                }
            }finally{
                lock.unlock();
            }
        }
    }
    
    public void suspend() throws SystemException{
        if(lock.tryLock()){
            try{
                manager.suspend();
            }finally{
                lock.unlock();
            }
        }
    }
    
    public void resume() throws SystemException,InvalidTransactionException{
        if(lock.tryLock()){
            try{
                manager.resume(transaction);
            }finally{
                lock.unlock();
            }
        }
    }
    
    private int getStatus() throws SystemException {
        if (done)
            return Status.STATUS_NO_TRANSACTION;
        return status;
    }
    
    public boolean isDone() {
        return done;
    }

    public boolean isEnlisted()	{
		return this.isEnlisted;
	}
    
    public synchronized void instanceDone() {
        status = Status.STATUS_NO_TRANSACTION;
        notifyAll();
        done = true;
    }
    
    // for registering synchronizations during transaction
    public void registerSynchronization(Synchronization s)
                                            throws RollbackException,
                                            java.lang.IllegalStateException,
                                            SystemException {
        if (s == null)
            throw new IllegalArgumentException("Null synchronization " + this);
        
        //get status of the transaction
        status = transaction.getStatus();
        
        if(lock.tryLock()){
            try {
                switch (status) {
                    case Status.STATUS_ACTIVE:
                    case Status.STATUS_PREPARING:
                        break;
                    case Status.STATUS_PREPARED:
                        throw new IllegalStateException("Already prepared. " + this);
                    case Status.STATUS_COMMITTING:
                        throw new IllegalStateException("Already started committing. " + this);
                    case Status.STATUS_COMMITTED:
                        throw new IllegalStateException("Already committed. " + this);
                    case Status.STATUS_MARKED_ROLLBACK:
                        throw new RollbackException("Already marked for rollback " + this);
                    case Status.STATUS_ROLLING_BACK:
                        throw new RollbackException("Already started rolling back. " + this);
                    case Status.STATUS_ROLLEDBACK:
                        throw new RollbackException("Already rolled back. " + this);
                    case Status.STATUS_NO_TRANSACTION:
                        throw new IllegalStateException("No transaction. " + this);
                    case Status.STATUS_UNKNOWN:
                        throw new IllegalStateException("Unknown state " + this);
                    default:
                        throw new IllegalStateException("Illegal status: " + TransactionStateUtility.getStringStatus(status) + " tx=" + this);
                }

                if (syncCount == syncAllocSize) {
                    syncAllocSize = 2 * syncAllocSize;
                    Synchronization[] sy = new Synchronization[syncAllocSize];
                    System.arraycopy(sync, 0, sy, 0, syncCount);
                    sync = sy;
                }
                sync[syncCount++] = s;
            } finally {
                lock.unlock();
            }
        }
    }
    
    public boolean delist(int flag)
                                     throws java.lang.IllegalStateException,
                                     SystemException, XAException{
        boolean isDelisted = false;
        if (xar == null)
            throw new IllegalArgumentException("null xaRes tx=" + this);
        if (flag != XAResource.TMSUCCESS &&
                flag != XAResource.TMSUSPEND &&
                flag != XAResource.TMFAIL)
            throw new IllegalArgumentException("Bad flag: " + flag + " tx=" + this);
        
        if(lock.tryLock()){
            try {
               status = transaction.getStatus();
                switch (status) {
                    case Status.STATUS_ACTIVE:
                    case Status.STATUS_MARKED_ROLLBACK:
                        break;
                    case Status.STATUS_PREPARING:
                        throw new IllegalStateException("Already started preparing. " + this);
                    case Status.STATUS_ROLLING_BACK:
                        throw new IllegalStateException("Already started rolling back. " + this);
                    case Status.STATUS_PREPARED:
                        throw new IllegalStateException("Already prepared. " + this);
                    case Status.STATUS_COMMITTING:
                        throw new IllegalStateException("Already started committing. " + this);
                    case Status.STATUS_COMMITTED:
                        throw new IllegalStateException("Already committed. " + this);
                    case Status.STATUS_ROLLEDBACK:
                        throw new IllegalStateException("Already rolled back. " + this);
                    case Status.STATUS_NO_TRANSACTION:
                        throw new IllegalStateException("No transaction. " + this);
                    case Status.STATUS_UNKNOWN:
                        throw new IllegalStateException("Unknown state " + this);
                    default:
                        throw new IllegalStateException("Illegal status: " + TransactionStateUtility.getStringStatus(status) + " tx=" + this);
                }
                try{
                    xar.end(xid,XAResource.TMSUCCESS);
                    isDelisted= transaction.delistResource(xar,flag);
                }catch(XAException xae){
                    xar.end(xid,XAResource.TMFAIL);
                    transaction.setRollbackOnly();
                    transaction.delistResource(xar,XAResource.TMFAIL);
                }
            } finally {
                lock.unlock();
            }
        }
        return isDelisted;
    }
    
    public void setRollbackOnly()
            throws java.lang.IllegalStateException,
            SystemException,
            XAException{
        status = transaction.getStatus();
        if(lock.tryLock()){
            try {
                switch (status) {
                    case Status.STATUS_ACTIVE:
                    case Status.STATUS_PREPARING:
                    case Status.STATUS_PREPARED:
                        status = Status.STATUS_MARKED_ROLLBACK;
                        xar.rollback(xid);
                        transaction.setRollbackOnly();
                    case Status.STATUS_MARKED_ROLLBACK:
                    case Status.STATUS_ROLLING_BACK:
                        xar.rollback(xid);
                        transaction.setRollbackOnly();
                        return;
                    case Status.STATUS_COMMITTING:
                        throw new IllegalStateException("Already started committing. " + this);
                    case Status.STATUS_COMMITTED:
                        throw new IllegalStateException("Already committed. " + this);
                    case Status.STATUS_ROLLEDBACK:
                        throw new IllegalStateException("Already rolled back. " + this);
                    case Status.STATUS_NO_TRANSACTION:
                        throw new IllegalStateException("No transaction. " + this);
                    case Status.STATUS_UNKNOWN:
                        throw new IllegalStateException("Unknown state " + this);
                    default:
                        throw new IllegalStateException("Illegal status: " + TransactionStateUtility.getStringStatus(status) + " tx=" + this);
                }
            } finally {
                lock.unlock();
            }
        }
    }
    
    // this needs to be brought into the code for 
    public void timedOut() throws SystemException{
        status = transaction.getStatus();
        if(lock.tryLock()){
            try {
                switch (status) {
                    case Status.STATUS_ROLLEDBACK:
                    case Status.STATUS_COMMITTED:
                    case Status.STATUS_NO_TRANSACTION:
                        return; 
                    case Status.STATUS_ROLLING_BACK:
                        return; 
                    case Status.STATUS_COMMITTING:
                        gotHeuristic(null, XAException.XA_HEURMIX);
                        status = Status.STATUS_MARKED_ROLLBACK;
                        return; // commit will fail
                    case Status.STATUS_PREPARED:
                    case Status.STATUS_ACTIVE:
                        status = Status.STATUS_MARKED_ROLLBACK;
                    case Status.STATUS_MARKED_ROLLBACK:
                        return;
                    case Status.STATUS_PREPARING:
                        status = Status.STATUS_MARKED_ROLLBACK;
                        return; // commit will fail
                    default:
                        return;
                }
            } catch(XAException xe){
                mLogger.log(Level.SEVERE,"Error While Setting the Transaction Status",xe);
                //xe.printStackTrace();
            }finally {     
                lock.unlock();
            }
        }
    }
    
    // need to do
    public void gotHeuristic(XAResource resource, int code) throws XAException{
        switch (code) {
            case XAException.XA_HEURMIX:
                heuristicCode = XAException.XA_HEURMIX;
                break;
            case XAException.XA_HEURRB:
                if (heuristicCode == HEUR_NONE)
                    heuristicCode = XAException.XA_HEURRB;
                else if (heuristicCode == XAException.XA_HEURCOM ||
                        heuristicCode == XAException.XA_HEURHAZ)
                    heuristicCode = XAException.XA_HEURMIX;
                break;
            case XAException.XA_HEURCOM:
                if (heuristicCode == HEUR_NONE)
                    heuristicCode = XAException.XA_HEURCOM;
                else if (heuristicCode == XAException.XA_HEURRB ||
                        heuristicCode == XAException.XA_HEURHAZ)
                    heuristicCode = XAException.XA_HEURMIX;
                break;
            case XAException.XA_HEURHAZ:
                if (heuristicCode == HEUR_NONE)
                    heuristicCode = XAException.XA_HEURHAZ;
                else if (heuristicCode == XAException.XA_HEURCOM ||
                        heuristicCode == XAException.XA_HEURRB)
                    heuristicCode = XAException.XA_HEURMIX;
                break;
            default:
                throw new IllegalArgumentException();
        }
        
        if (resource != null)
            resource.forget(xid);
    } 
    
    public void checkHeuristics()
            throws HeuristicMixedException, HeuristicRollbackException {
        switch (heuristicCode) {
            case XAException.XA_HEURHAZ:
            case XAException.XA_HEURMIX:
                heuristicCode = HEUR_NONE;
                throw new HeuristicMixedException();
            case XAException.XA_HEURRB:
                heuristicCode = HEUR_NONE;
                throw new HeuristicRollbackException();
            case XAException.XA_HEURCOM:
                heuristicCode = HEUR_NONE;
                return;
        }
    } 
    
    private static final int HEUR_NONE = XAException.XA_RETRY;
    private XidImpl xid;
    private GlobalId gid;
    //callback synchronizations
    private Synchronization[] sync = new Synchronization[3];
    private int syncAllocSize = 3;
    private int syncCount = 0;
    private ArrayList resources = new ArrayList(3);
    private int heuristicCode = HEUR_NONE;
    private int status;
    private long start;
    private long timeoutPeriod;
      
    private boolean done = false;
    private Throwable exceptionReason;
    
    public static XidFactory xidFactory = new XidFactory();//XidFactory.getXidFactoryInstance();
    
    private final Lock lock = new ReentrantLock(true);
    private String tx_enable = System.getProperty("com.sun.enterprise.jbi.tx.enable");
    
    TransactionManager manager = null;
    Transaction transaction = null;
    XADataSource xads = null;
    XAResource xar = null;
    XAConnection xac = null;
	boolean isEnlisted = false;

    
    ComponentContext componentContext = null;
    MessageExchange messageExchange = null;
}
