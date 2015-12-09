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
 * @(#)TransactionStateUtility.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.transaction;


import com.sun.jbi.internationalization.Messages;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.transaction.Status;
import javax.transaction.SystemException;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.transaction.UserTransaction;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
/**
 *
 * @author Venkat P
 */
public class TransactionStateUtility {
    
    private static final Messages mMessages = Messages.getMessages(TransactionStateUtility.class);

    private static final Logger mLogger = Messages.getLogger(TransactionStateUtility.class);
    
    /** Creates a new instance of TransactionStateUtility */
    private TransactionStateUtility() {
    }
    
    /** Transaction Status Strings */
    private static final String[] TxStatusStrings =
    {
        "STATUS_ACTIVE",
        "STATUS_MARKED_ROLLBACK",
        "STATUS_PREPARED",
        "STATUS_COMMITTED",
        "STATUS_ROLLEDBACK",
        "STATUS_UNKNOWN",
        "STATUS_NO_TRANSACTION",
        "STATUS_PREPARING",
        "STATUS_COMMITTING",
        "STATUS_ROLLING_BACK"
    };
    
    private static boolean isActive(Transaction tx) throws Exception{
        try {
            return tx != null && (tx.getStatus() == Status.STATUS_ACTIVE);
        } catch (SystemException error) {
            mLogger.log(Level.SEVERE,"Error While Setting the Transaction Status");
            throw new Exception(error);
        }
    }
    
    private static boolean isActive(TransactionManager tm) throws Exception{
        try {
            return isActive(tm.getTransaction());
        } catch (SystemException error) {
            mLogger.log(Level.SEVERE,"Error While Setting the Transaction Status");
            throw new Exception(error);
        }
    }
    
    private static boolean isActive(UserTransaction ut) throws Exception{
        try {
            return ut.getStatus() == Status.STATUS_ACTIVE;
        } catch (SystemException error) {
            mLogger.log(Level.SEVERE,"Error While Setting the Transaction Status");
            throw new Exception(error);
        }
    }
    
    private static boolean isUncommitted(Transaction tx) throws Exception{
        try {
            if (tx == null)
                return false;
            int status = tx.getStatus();
            return status == Status.STATUS_ACTIVE || status == Status.STATUS_MARKED_ROLLBACK;
        } catch (SystemException error) {
            mLogger.log(Level.SEVERE,"Error While Setting the Transaction Status");
            throw new Exception(error);
        }
    }
    
    private static boolean isUncommitted(TransactionManager tm)throws Exception {
        try {
            return isUncommitted(tm.getTransaction());
        } catch (SystemException error) {
            mLogger.log(Level.SEVERE,"Error While Setting the Transaction Status");
            throw new Exception(error);
        }
    }
    
    private static boolean isUncommitted(UserTransaction ut) throws Exception {
        try {
            int status = ut.getStatus();
            return status == Status.STATUS_ACTIVE || status == Status.STATUS_MARKED_ROLLBACK;
        } catch (SystemException error) {
            throw new Exception(error);
        }
    }
    
    private static boolean isCompleted(Transaction tx) throws Exception{
        try {
            if (tx == null)
                return true;
            int status = tx.getStatus();
            return status == Status.STATUS_COMMITTED
                    || status == Status.STATUS_ROLLEDBACK
                    || status == Status.STATUS_NO_TRANSACTION;
        } catch (SystemException error) {
            mLogger.log(Level.SEVERE,"Error While Setting the Transaction Status");
            throw new Exception(error);
        }
    }
    
    private static boolean isCompleted(TransactionManager tm) throws Exception{
        try {
            return isCompleted(tm.getTransaction());
        } catch (SystemException error) {
            mLogger.log(Level.SEVERE,"Error While Setting the Transaction Status");
            throw new Exception(error);
        }
    }
    
    private static boolean isCompleted(UserTransaction ut) throws Exception{
        try {
            int status = ut.getStatus();
            return status == Status.STATUS_COMMITTED
                    || status == Status.STATUS_ROLLEDBACK
                    || status == Status.STATUS_NO_TRANSACTION;
        } catch (SystemException error) {
            mLogger.log(Level.SEVERE,"Error While Setting the Transaction Status");
            throw new Exception(error);
        }
    }
    
    /**
     * Converts a tx Status index to a String
     *
     * @see javax.transaction.Status
     *
     * @param status the Status index
     * @return status as String or "STATUS_INVALID"
     */
    private static String getStatusAsString(int status) {
        if (status >= Status.STATUS_ACTIVE && status <= Status.STATUS_ROLLING_BACK) {
            return TxStatusStrings[status];
        } else {
            return "STATUS_INVALID";
        }
    }
    
    /**
     * Converts a XAResource flag to a String
     *
     * @see javax.transaction.xa.XAResource
     *
     * @param flags the flags passed in to start(), end(), recover()
     * @return the flags in String form
     */
    private static String getXAResourceFlagsAsString(int flags) {
        if (flags == XAResource.TMNOFLAGS) {
            return "|TMNOFLAGS";
        } else {
            StringBuffer sbuf = new StringBuffer(64);
            
            if ((flags & XAResource.TMONEPHASE) != 0) {
                sbuf.append("|TMONEPHASE");
            }
            if ((flags & XAResource.TMJOIN) != 0) {
                sbuf.append("|TMJOIN");
            }
            if ((flags & XAResource.TMRESUME) != 0) {
                sbuf.append("|TMRESUME");
            }
            if ((flags & XAResource.TMSUCCESS) != 0) {
                sbuf.append("|TMSUCCESS");
            }
            if ((flags & XAResource.TMFAIL) != 0) {
                sbuf.append("|TMFAIL");
            }
            if ((flags & XAResource.TMSUSPEND) != 0) {
                sbuf.append("|TMSUSPEND");
            }
            if ((flags & XAResource.TMSTARTRSCAN) != 0) {
                sbuf.append("|TMSTARTRSCAN");
            }
            if ((flags & XAResource.TMENDRSCAN) != 0) {
                sbuf.append("|TMENDRSCAN");
            }
            return sbuf.toString();
        }
    }
    
     /**
     *  Return a string representation of the given status code.
     */
    static String getStringStatus(int status) {
        switch (status) {
            case Status.STATUS_PREPARING:
                return "STATUS_PREPARING";
            case Status.STATUS_PREPARED:
                return "STATUS_PREPARED";
            case Status.STATUS_ROLLING_BACK:
                return "STATUS_ROLLING_BACK";
            case Status.STATUS_ROLLEDBACK:
                return "STATUS_ROLLEDBACK";
            case Status.STATUS_COMMITTING:
                return "STATUS_COMMITING";
            case Status.STATUS_COMMITTED:
                return "STATUS_COMMITED";
            case Status.STATUS_NO_TRANSACTION:
                return "STATUS_NO_TRANSACTION";
            case Status.STATUS_UNKNOWN:
                return "STATUS_UNKNOWN";
            case Status.STATUS_MARKED_ROLLBACK:
                return "STATUS_MARKED_ROLLBACK";
            case Status.STATUS_ACTIVE:
                return "STATUS_ACTIVE";
            default:
                return "STATUS_UNKNOWN(" + status + ")";
        }
    }
    
    /**
     *  Return a string representation of the given XA error code.
     */
    private static String getStringXAErrorCode(int errorCode) {
        switch (errorCode) {
            case XAException.XA_HEURCOM:
                return "XA_HEURCOM";
            case XAException.XA_HEURHAZ:
                return "XA_HEURHAZ";
            case XAException.XA_HEURMIX:
                return "XA_HEURMIX";
            case XAException.XA_HEURRB:
                return "XA_HEURRB";
            case XAException.XA_NOMIGRATE:
                return "XA_NOMIGRATE";
            case XAException.XA_RBCOMMFAIL:
                return "XA_RBCOMMFAIL";
            case XAException.XA_RBDEADLOCK:
                return "XA_RBDEADLOCK";
            case XAException.XA_RBINTEGRITY:
                return "XA_RBINTEGRITY";
            case XAException.XA_RBOTHER:
                return "XA_RBOTHER";
            case XAException.XA_RBPROTO:
                return "XA_RBPROTO";
            case XAException.XA_RBROLLBACK:
                return "XA_RBROLLBACK";
            case XAException.XA_RBTIMEOUT:
                return "XA_RBTIMEOUT";
            case XAException.XA_RBTRANSIENT:
                return "XA_RBTRANSIENT";
            case XAException.XA_RDONLY:
                return "XA_RDONLY";
            case XAException.XA_RETRY:
                return "XA_RETRY";
            case XAException.XAER_ASYNC:
                return "XAER_ASYNC";
            case XAException.XAER_DUPID:
                return "XAER_DUPID";
            case XAException.XAER_INVAL:
                return "XAER_INVAL";
            case XAException.XAER_NOTA:
                return "XAER_NOTA";
            case XAException.XAER_OUTSIDE:
                return "XAER_OUTSIDE";
            case XAException.XAER_PROTO:
                return "XAER_PROTO";
            case XAException.XAER_RMERR:
                return "XAER_RMERR";
            case XAException.XAER_RMFAIL:
                return "XAER_RMFAIL";
            default:
                return "XA_UNKNOWN(" + errorCode + ")";
        }
    }
    
}
