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
 * @(#)TransactionInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist;

import java.sql.Connection;

import javax.transaction.Synchronization;
import javax.transaction.Transaction;

public class TransactionInfo {
    private StateManager.TransactionType transactionType;
    private Transaction transaction;
    private Synchronization synchronization;
    private boolean onlyTxOperation; 
    private Connection connection;
    
    private static final TransactionInfo LOCAL_TX_INFO = 
        new TransactionInfo(StateManager.TransactionType.Local); 
    
    public static TransactionInfo getLocalTxInfo() {
        return LOCAL_TX_INFO;
    }
    
    private TransactionInfo(StateManager.TransactionType transactionType) {
        this.transactionType = transactionType;
    }

    public TransactionInfo(StateManager.TransactionType transactionType, Transaction transaction, 
            Synchronization synchronization, boolean onlyTxOperation) {
        this.transactionType = transactionType;
        this.transaction = transaction;
        this.synchronization = synchronization;
        this.onlyTxOperation = onlyTxOperation;
    }

    /**
     * @return Returns the synchronization.
     */
    public Synchronization getSynchronization() {
        return synchronization;
    }

    /**
     * @param synchronization The synchronization to set.
     */
    public void setSynchronization(Synchronization synchronization) {
        this.synchronization = synchronization;
    }

    /**
     * @return Returns the transaction.
     */
    public Transaction getTransaction() {
        return transaction;
    }

    /**
     * @param transaction The transaction to set.
     */
    public void setTransaction(Transaction transaction) {
        this.transaction = transaction;
    }

    /**
     * @return Returns the transactionType.
     */
    public StateManager.TransactionType getTransactionType() {
        return transactionType;
    }

    /**
     * @param transactionType The transactionType to set.
     */
    public void setTransactionType(StateManager.TransactionType transactionType) {
        this.transactionType = transactionType;
    }

    /**
     * @return Returns the onlyTxOperation.
     */
    public boolean isOnlyTxOperation() {
        return onlyTxOperation;
    }

    /**
     * @param onlyTxOperation The onlyTxOperation to set.
     */
    public void setOnlyTxOperation(boolean onlyTxOperation) {
        this.onlyTxOperation = onlyTxOperation;
    }

    /**
     * @return the connection
     */
    public Connection getConnection() {
        return connection;
    }

    /**
     * @param connection The connection to set
     */
    public void setConnection(Connection connection) {
        this.connection = connection;
    }
}
