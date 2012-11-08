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
 * @(#)TransactionsUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.util;

import com.sun.jbi.httpsoapbc.HttpSoapComponentContext;
import com.sun.jbi.internationalization.Messages;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.transaction.InvalidTransactionException;
import javax.transaction.SystemException;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;


public class TransactionsUtil {
    
    private static final Messages mMessages =
        Messages.getMessages(TransactionsUtil.class);
    private static final Logger mLogger =
        Messages.getLogger(TransactionsUtil.class);    
    
    private TransactionsUtil() {
    }
    
    private static TransactionManager getTransactionManager() {
        // This down-cast is necessary and guaranteed to succeed;
        // see javax.jbi.component.ComponentContext#getTransactionManager documentation.
        return (TransactionManager) HttpSoapComponentContext.getInstance().getContext().getTransactionManager();
    }
    
    /**
     * Suspends the transaction associated with this current thread.
     * 
     * @returns A reference to the transaction that was suspended, may be
     *          null if no transaction was previously associated to the thread.
     * 
     * @throws SystemException if the TransactionManager encounters an
     *                         unexpected error condition.
     */
    public static Transaction suspendTransaction() throws SystemException {
        Transaction transaction = null;
        TransactionManager tm = getTransactionManager();
        // If there is no transaction manager, it means non-transacted activity.
        // Don't panic, proceed without it.
        if (tm != null) {
            try {
                // Suspend current transaction
                transaction = tm.getTransaction();
                if (transaction != null) {
                    tm.suspend();
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log (Level.FINE, "Suspended transaction [" + transaction + "]");
                    }
                }
            } catch (SystemException t) {
                String errmsg2 = mMessages.getString(
                        "HTTPBC-E00680.Transaction_suspend_failed");
                mLogger.log(Level.SEVERE, errmsg2, t);    
                throw t;
            }
        }
        return transaction;
    }
    
    /**
     * Resumes the specified transaction.
     * 
     * @param transaction The transaction to associate to the current thread.
     * 
     * @returns true if a transaction is specified, and is successfully resumed.
     *               If a null transaction is specified, or a non-transactional
     *               environment exists, <code>false</code> is returned.
     */
    public static boolean resumeTransaction(Transaction transaction) {
        boolean txResumed = false;
        if (transaction != null) {
            TransactionManager tm = getTransactionManager();
            // If there is no transaction manager, it just means a non-transacted activity.
            // Don't panic, proceed without it.
            if (tm != null) {
                // Resume the current transaction
                try {
                    tm.resume(transaction);
                    txResumed = true;
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log (Level.FINE, "Resumed transaction [" + transaction + "]");
                    }
                } catch (Throwable t) {
                    String errmsg2 = mMessages.getString(
                            "HTTPBC-E00681.Transaction_resume_failed",
                            new Object[]{transaction});
                    mLogger.log(Level.SEVERE, errmsg2, t);
                }
            }
        }
        return txResumed;
    }
    
    public static boolean setRollbackOnlyOnTransaction(Transaction transaction) {
        boolean success = false;
        if (transaction != null) {
            try {
                transaction.setRollbackOnly();
                success = true;
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log (Level.FINE, "Called setRollbackOnly on transaction [" + transaction + "]");
                }
            } catch (Throwable t) {
                String errmsg2 = mMessages.getString(
                        "HTTPBC-E00683.Transaction_setRollbackOnly_failed",
                        new Object[]{transaction});
                mLogger.log(Level.SEVERE, errmsg2, t);
            }
        }
        return success;
    }
}
