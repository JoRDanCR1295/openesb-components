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
package org.glassfish.openesb.pojose.core.util;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import org.glassfish.openesb.pojose.res.impl.CallTracker;

/**
 *
 * @author gpatil
 */
public class TransactionHelper {
    private static String nameResume = "resume" ; //NOI18N
    private static String nameSuspend = "suspend" ; //NOI18N
    private static String nameSetRollbackOnly = "setRollbackOnly" ; //NOI18N
    private static String nameGetTransaction = "getTransaction";
    
    private static Method methodResume = null;
    private static Method methodSuspend = null;
    private static Method methodSetRollbackOnly = null;
    private static Method methodGetTransaction = null;

    /**
     * <b>Execute through reflection</b>. We do not want to keep comiple and runtime
     * dependency with JTA API classes. JBI API and runtime also follow the
     * same to have flexibility in running non-txn supported environemt.
     * 
     * @param tm
     * @param tx
     */
    public static void resumeTransaction(Object tm, MessageExchange me){
        Object tx = me.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
        if ((tm != null) && (tx != null)) {
            if (methodResume == null || methodSetRollbackOnly == null){
                synchronized(TransactionHelper.class){
                    if (methodResume == null){
                        Method[] ms = tm.getClass().getMethods();
                        Method m = null;
                        for (int i = 0; i < ms.length; i++){
                            m = ms[i];
                            if (nameResume.equals(m.getName())){
                                methodResume = m;
                                break;
                            }
                        }
                    }
                    
                    if (methodSetRollbackOnly == null){
                        Method[] ms = tm.getClass().getMethods();
                        Method m = null;
                        for (int i = 0; i < ms.length; i++){
                            m = ms[i];
                            if (nameSetRollbackOnly.equals(m.getName())){
                                methodSetRollbackOnly = m;
                                break;
                            }
                        }
                    }
                }
            }

            try {
                if(me.getStatus().equals(ExchangeStatus.ERROR)) {
                    methodSetRollbackOnly.invoke(tx, null);
                }
                
                methodResume.invoke(tm, new Object[]{tx});
            } catch (IllegalAccessException ex) {
                Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
            } catch (IllegalArgumentException ex) {
                Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
            } catch (InvocationTargetException ex) {
                Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }

    public static Object suspendTransaction(Object tm, MessageExchange me){
        Object retTxn = null;

        if (tm != null) {
            if (methodSuspend == null || methodSetRollbackOnly == null || methodGetTransaction == null){
                synchronized(TransactionHelper.class){
                    if (methodSuspend == null){
                        try {
                            methodSuspend = tm.getClass().getMethod(nameSuspend, null);
                        } catch (NoSuchMethodException ex) {
                            Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
                        } catch (SecurityException ex) {
                            Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
                        }
                    }
                    
                    if (methodSetRollbackOnly == null){
                        Method[] ms = tm.getClass().getMethods();
                        Method m = null;
                        for (int i = 0; i < ms.length; i++){
                            m = ms[i];
                            if (nameSetRollbackOnly.equals(m.getName())){
                                methodSetRollbackOnly = m;
                                break;
                            }
                        }
                    }
                    
                    if (methodGetTransaction == null){
                        Method[] ms = tm.getClass().getMethods();
                        Method m = null;
                        for (int i = 0; i < ms.length; i++){
                            m = ms[i];
                            if (nameGetTransaction.equals(m.getName())){
                                methodGetTransaction = m;
                                break;
                            }
                        }
                    }
                }
            }

            try {
                Object tx = methodGetTransaction.invoke(tm, null);
                
                if(me.getStatus().equals(ExchangeStatus.ERROR)) {
                    methodSetRollbackOnly.invoke(tx, null);
                }
                
                retTxn = methodSuspend.invoke(tm, null);
            } catch (IllegalAccessException ex) {
                Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
            } catch (IllegalArgumentException ex) {
                Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
            } catch (InvocationTargetException ex) {
                Logger.getLogger(Util.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        
        return retTxn;
    }

//    public static boolean suspendForPropagation(Object tm, MessageExchange consME, CallTracker ct){
//        boolean suspended = false;
//        //If not already suspended for another consuming ME,
//        if (ct.isOkToPropagateTxn(consME)){ //Mutable operation
//            suspendTransaction(tm);
//            suspended = true;
//        }
//
//        return suspended;
//    }

    public static void checkAndResumeForConsumedME(Object tm, Object txn, MessageExchange consME, CallTracker ct){
        if (ct.isOkToResumeTxn(consME)){ 
            resumeTransaction(tm, consME);
        }
    }

    public static void suspendIfStartedForConsumedME(Object tm, MessageExchange me, CallTracker ct){
        // No need to check. Suspend will return null if txn is not associated with the thread.
        suspendTransaction(tm, me);
        //ct.disassociateTxnFromConsumerME(me);
    }
}
