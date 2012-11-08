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
 * @(#)DummyTransaction.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.jbiadaptor.test.common;

import java.sql.Connection;
import java.sql.SQLException;

import javax.transaction.HeuristicMixedException;
import javax.transaction.HeuristicRollbackException;
import javax.transaction.RollbackException;
import javax.transaction.Status;
import javax.transaction.Synchronization;
import javax.transaction.SystemException;
import javax.transaction.Transaction;
import javax.transaction.xa.XAResource;

public class DummyTransaction implements Transaction {
    
    private DummyTxManagerAndDataSource dummyTMAndDS;
    
    public DummyTransaction (DummyTxManagerAndDataSource dummyTMAndDS) {
        this.dummyTMAndDS = dummyTMAndDS;
    }
    
    private Connection connection = null;
    private Synchronization synchronization;
    
    /**
     * @param mConn the mConn to set
     */
    public void setConnection(Connection connection) {
        this.connection = connection;
    }



    /**
     * @see javax.transaction.Transaction#commit()
     */
    public void commit() throws RollbackException, HeuristicMixedException, HeuristicRollbackException, SecurityException, IllegalStateException, SystemException {
        
        try {
            if (connection != null) {
                connection.commit();
                if (dummyTMAndDS.mIsClusteredTests) {
                    connection.close();
                }
            }
            
            if (synchronization != null){
                synchronization.afterCompletion(Status.STATUS_COMMITTED);
            }
            
            dummyTMAndDS.mTxMap.remove(Thread.currentThread().getName());

        } catch (SQLException e) {
            throw new RuntimeException (e);
        }
    }

    /**
     * @see javax.transaction.Transaction#rollback()
     */
    public void rollback() throws IllegalStateException, SystemException {
        try {
            
            if (connection != null) {
                dummyTMAndDS.mTxMap.remove(Thread.currentThread().getName());       
                connection.rollback();
                if (dummyTMAndDS.mIsClusteredTests) {
                    connection.close();
                }
            }
            
            if (synchronization != null){
                synchronization.afterCompletion(Status.STATUS_ROLLEDBACK);            
            }
            
        } catch (SQLException e) {
            throw new RuntimeException (e);
        }        
    }

    /**
     * @see javax.transaction.Transaction#registerSynchronization(javax.transaction.Synchronization)
     */
    public void registerSynchronization(Synchronization arg0) throws RollbackException, IllegalStateException, SystemException {
        synchronization = arg0;
    }
    
    /**
     * @see javax.transaction.Transaction#delistResource(javax.transaction.xa.XAResource, int)
     */
    public boolean delistResource(XAResource arg0, int arg1) throws IllegalStateException, SystemException {
        return false;
    }

    /**
     * @see javax.transaction.Transaction#enlistResource(javax.transaction.xa.XAResource)
     */
    public boolean enlistResource(XAResource arg0) throws RollbackException, IllegalStateException, SystemException {
        return false;
    }

    /**
     * @see javax.transaction.Transaction#getStatus()
     */
    public int getStatus() throws SystemException {
        return 0;
    }

    /**
     * @see javax.transaction.Transaction#setRollbackOnly()
     */
    public void setRollbackOnly() throws IllegalStateException, SystemException {
    }
}
