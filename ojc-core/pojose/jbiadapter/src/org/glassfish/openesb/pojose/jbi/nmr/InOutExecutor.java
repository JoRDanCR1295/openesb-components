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
 * @(#)InOutExecutor.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.jbi.nmr;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.messaging.MessageExchange;
import org.glassfish.openesb.pojose.api.ErrorMessage;
import org.glassfish.openesb.pojose.api.FaultMessage;
import org.glassfish.openesb.pojose.api.MessageException;
import org.glassfish.openesb.pojose.core.anno.meta.POJOClassMetadata;
import org.glassfish.openesb.pojose.core.util.TransactionHelper;
import org.glassfish.openesb.pojose.jbi.POJOComponentContext;
import org.glassfish.openesb.pojose.jbi.su.MasterTracker;
import org.glassfish.openesb.pojose.jbi.su.ProviderTracker;

/**
 *
 * @author gpatil
 */
public class InOutExecutor extends BasePojoExecutor {

    public InOutExecutor(POJOComponentContext ctx, POJOClassMetadata classMeta,
            Object pojo){
        super(ctx, classMeta, pojo);
    }

    public void executePojoOperation(MessageExchange provME){
        Method m = opm.getMethod();
        Exception exp = null;
        boolean error = false;

        ProviderTracker pt = MasterTracker.getInstance().getProviderTracker(provME);
        ClassLoader thdCtxCl = Thread.currentThread().getContextClassLoader();

        try {
            // Ctx CL
            Thread.currentThread().setContextClassLoader(pojo.getClass().getClassLoader());

            // Ctx Txn. Set the Transaction context.
            if (provME.isTransacted()){
                TransactionHelper.resumeTransaction(ctx.getJBIComponentContext().getTransactionManager(),
                        provME);
            }
            
            Object[] args = getInputArguments(provME, m, opm);
            Object retObj = m.invoke(pojo, args);
            
            if (!meta.isValidOnDonePresent()){
                updateOutMessage(provME, retObj, opm);
            }
        } catch (InvocationTargetException ite){
            Throwable cause = ite.getCause();
            if (cause instanceof ErrorMessage){
                error = true;
                exp = (ErrorMessage) cause;
            } else if (cause instanceof FaultMessage){
                exp = (MessageException) cause;
            }else if (cause instanceof Exception){
                error = true;
                exp = (Exception) cause;
                Logger.getLogger(BasePojoExecutor.class.getName()).log(Level.SEVERE, null, cause);
            } else {
                error = true;
                exp = new Exception(cause);
                Logger.getLogger(BasePojoExecutor.class.getName()).log(Level.SEVERE, null, cause);
            }
        } catch (Exception ex){
            error = true;
            exp = ex;
        } finally {
            // Before sending ME back, reset CL and Txn context.
            // Ctx CL
            if (thdCtxCl != null){
                Thread.currentThread().setContextClassLoader(thdCtxCl);
            }

            // Ctx Txn
            if (provME.isTransacted()){
                TransactionHelper.suspendTransaction(ctx.getJBIComponentContext().getTransactionManager(), provME);
            }
            pt.setExecutedOperation();
        }

        // If @OnDone not present or error or exception occured, end the exchange.
        if ((!meta.isValidOnDonePresent()) || (exp != null) ){
            try {
                if (exp != null){
                    if (error){
                        provME.setError(exp);
                    }else{
                        provME.setFault(((FaultMessage)exp).getFault());
                    }
                }

                ctx.getDC().send(provME);
            } catch (Exception ex) {
                exp = ex;
                Logger.getLogger(BasePojoExecutor.class.getName()).log(Level.SEVERE, null, ex);
            } finally {
                if (error || (exp != null)){
                    pt.setDoneWithProvisioning();
                }else{
                    pt.setSentProviderResponse();
                }
            }
        } else {
            if (pt.canExecuteOnDone()){
                if (pt.tryAcquireLockForOnDoneExec()){
                    try{
                        this.executeOnDone(provME);
                    } finally {
                        pt.releaseLockForOnDoneExec();
                    }
                }
            }
        }
    }

    public void executeOnDone(MessageExchange provME) {
        Exception exp = null;
        boolean error = false;
        ProviderTracker pt = MasterTracker.getInstance().getProviderTracker(provME);
        ClassLoader thdCtxCl = Thread.currentThread().getContextClassLoader();

        try {
            Method onDone = this.meta.getOnDoneMetadata().getMethod();
            // Ctx CL
            Thread.currentThread().setContextClassLoader(pojo.getClass().getClassLoader());
            // Ctx Txn. Set the Transaction context.
            if (provME.isTransacted()){
                TransactionHelper.resumeTransaction(ctx.getJBIComponentContext().getTransactionManager(),
                        provME);
            }
            
            Object retObj = onDone.invoke(pojo, null);
            // Use onDone returnType
            updateOutMessage(provME, retObj, opm, onDone);
        } catch (InvocationTargetException ite){
            Throwable cause = ite.getCause();
            if (cause instanceof ErrorMessage){
                error = true;
                exp = (ErrorMessage) cause;
            } else if (cause instanceof FaultMessage){
                exp = (MessageException) cause;
            }else if (cause instanceof Exception){
                error = true;
                exp = (Exception) cause;
            } else {
                error = true;
                exp = new Exception(cause);
            }
            Logger.getLogger(BasePojoExecutor.class.getName()).log(Level.SEVERE, null, cause);
        } catch (Exception ex){
            error = true;
            exp = ex;
        } finally {
            // Before sending ME back, reset CL and Txn context.
            // Ctx CL
            if (thdCtxCl != null){
                Thread.currentThread().setContextClassLoader(thdCtxCl);
            }
            
            // Ctx Txn
            if (provME.isTransacted()){
                TransactionHelper.suspendTransaction(ctx.getJBIComponentContext().getTransactionManager(), provME);
            }
        }

        try {
            if (exp != null){
                if (error){
                    provME.setError(exp);
                }else{
                    provME.setFault(((FaultMessage)exp).getFault());
                }
            }

            ctx.getDC().send(provME);
        } catch (Exception ex) {
            exp = ex;
            Logger.getLogger(BasePojoExecutor.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            pt.setExecutedOnDone();
        }
    }
}
