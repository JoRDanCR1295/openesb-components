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
 * @(#)JMSXARecovery.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.mqbc.recovery;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.component.ComponentContext;
import javax.transaction.xa.XAResource;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.mqbc.LogSupport;

/**
 *
 * Class abstracting XA transaction recovery with JBI framework
 */
public class MQXARecovery {
    
    public static final String COM_SUN_JBI_FRAMEWORK_COMPONENT_CONTEXT = "com.sun.jbi.framework.ComponentContext";
    private static final String METHOD_registerXAResource = "registerXAResource";
    
    private static final Messages mMessages =
        Messages.getMessages(MQXARecovery.class);
    private static final Logger mLogger =
        Messages.getLogger(MQXARecovery.class);
    
    private ComponentContext componentContext = null;
    private ConnectionInfoPersister persister = null;
    private MQXARecoveryHelper helper = null;
    private boolean recoverableComponentContext = false;
    
    /** Creates a new instance of JMSXARecovery */
    public MQXARecovery(ComponentContext componentContext,
                         ConnectionInfoPersister persister,
                         MQXARecoveryHelper helper) {
        this.componentContext = componentContext;
        this.persister = persister;
        this.helper = helper;
        this.recoverableComponentContext = 
                xaRecoverySupported(this.componentContext);
    }
    
    public void recover() throws MQXARecoveryException {
        if (recoverableComponentContext) {
            try {
                ArrayList recsRecovered = new ArrayList();
                ConnectionInfoRecord [] recsToRecover = persister.retrieve();
                mLogger.log(Level.INFO, 
                            "MQXARecovery_RECOVERY_MQCLIENTAGENT_REC_COUNT",
                            new Object[] {recsToRecover.length});
                if (recsToRecover.length > 0) {
                    for (int i=0; i < recsToRecover.length; i++) {
                        
                        MQConnectionInfoRecord rec = (MQConnectionInfoRecord)recsToRecover[i];
                        try {
                            XAResource xar = helper.getXAResource(rec);
                            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                                mLogger.log(LogSupport.LEVEL_DEBUG,
                                            "MQXARecovery_RECOVERY_MQCLIENTAGENT_CONNECTION_ESTABLISHED",
                                            new Object[]{rec.getQueueManagerName()});
                            }

                            Class com_sun_jbi_framework_ComponentContext_Class = componentContext.getClass();
                            Class[] parameterTypes = new Class[] {XAResource.class};
                            Method registerXAResource_Method = com_sun_jbi_framework_ComponentContext_Class.getMethod("registerXAResource", parameterTypes);                                
                            Object[] arguments = new Object[] {xar};
                            registerXAResource_Method.invoke(componentContext, arguments);
                            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                                mLogger.log(LogSupport.LEVEL_DEBUG,
                                            "MQXARecovery_RECOVERY_REGISTERED_XARESOURCE",
                                            new Object[]{rec.getQueueManagerName()});
                            }
                            recsRecovered.add(rec);
                        } catch (Throwable t) {
                            mLogger.log(Level.WARNING,
                                        "MQXARecovery_RECOVERY_REGISTER_XARESOURCE_FAILED",
                                        new Object[] {rec.getQueueManagerName(),
                                                      LogSupport.getStackTraceAsString(t)});
                            continue;
                        }
                    }
                    persister.remove((ConnectionInfoRecord [])recsRecovered.toArray(new ConnectionInfoRecord[recsRecovered.size()]));
                }
            } catch (Throwable t) {
                String stackTr = LogSupport.getStackTraceAsString(t);
                mLogger.log(Level.SEVERE,
                            "JMSXARecovery_RECOVERY_UNEXPECTED_ERROR",
                            new Object[]{stackTr});

                String errMsg = mMessages.getString(
                            "JMSXARecovery_RECOVERY_UNEXPECTED_ERROR",
                            new Object[]{stackTr});

                throw new MQXARecoveryException(errMsg, t);            
            }
        } else {
            mLogger.log(Level.WARNING,
                        "JMSXARecovery_RECOVERY_NOT_SUPPORTED",
                        new Object[] {componentContext.getClass().getName()});
        }
    }

    public static boolean xaRecoverySupported (ComponentContext cc) {
        boolean ret = false;
        try {
            Method [] methods = cc.getClass().getMethods();
            for (int i=0; i < methods.length; i++) {
                if (methods[i].getName().equals(METHOD_registerXAResource)) {
                    Class [] paramTypes = methods[i].getParameterTypes();
                    if (paramTypes != null && paramTypes.length == 1 && 
                        paramTypes[0].getName().equals(XAResource.class.getName())) {
                        ret = true;
                        break;
                    }
                }
            }
        } catch (Throwable t) {
            ret = false;
        }
        return ret;
    }
}
