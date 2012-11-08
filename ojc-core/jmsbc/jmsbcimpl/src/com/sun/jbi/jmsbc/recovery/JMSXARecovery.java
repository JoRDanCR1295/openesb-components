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

package com.sun.jbi.jmsbc.recovery;

import javax.jbi.component.ComponentContext;

import java.lang.reflect.Method;

import java.util.ArrayList;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.transaction.xa.XAResource;

import com.sun.jbi.jmsbc.LogSupport;
import com.sun.jbi.jmsbc.util.AlertsUtil;
import com.sun.jbi.jmsbc.JMSBindingComponent;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.internationalization.Messages;

/**
 *
 * Class abstracting XA transaction recovery with JBI framework
 */
public class JMSXARecovery {
    
    public static final String COM_SUN_JBI_FRAMEWORK_COMPONENT_CONTEXT = "com.sun.jbi.framework.ComponentContext";
    public static final String METHOD_registerXAResource = "registerXAResource";
    
    private static final Messages mMessages =
        Messages.getMessages(JMSXARecovery.class);
    private static final Logger mLogger =
        Messages.getLogger(JMSXARecovery.class);
    
    private ComponentContext componentContext = null;
    private ConnectionInfoPersister persister = null;
    private JMSXARecoveryHelper helper = null;
    private boolean recoverableComponentContext = false;
    
    /** Creates a new instance of JMSXARecovery */
    public JMSXARecovery(ComponentContext componentContext,
                         ConnectionInfoPersister persister,
                         JMSXARecoveryHelper helper) {
        this.componentContext = componentContext;
        this.persister = persister;
        this.helper = helper;
        this.recoverableComponentContext = 
                xaRecoverySupported(this.componentContext);
    }
    
    public void recover() throws JMSXARecoveryException {
        if (recoverableComponentContext) {
            try {
                ArrayList recsRecovered = new ArrayList();
                ConnectionInfoRecord [] recsToRecover = persister.retrieve();
                if (mLogger.isLoggable(Level.INFO)) {
                    mLogger.log(Level.INFO, 
                                "JMSBC-I0501.ConnectionInfoRecordsRetrieved",
                                new Object[] {recsToRecover.length});
                }
                if (recsToRecover.length > 0) {
                    for (int i=0; i < recsToRecover.length; i++) {
                        
                        JMSConnectionInfoRecord rec = (JMSConnectionInfoRecord)recsToRecover[i];
                        try {
                            XAResource xar = helper.getXAResource(rec);
                            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                                mLogger.log(LogSupport.LEVEL_DEBUG,
                                            "JMSXARecovery_RECOVERY_CONNECTION_ESTABLISHED",
                                            new Object[]{rec.getConnectionURL()});
                            }

                            Class com_sun_jbi_framework_ComponentContext_Class = componentContext.getClass();
                            Class[] parameterTypes = new Class[] {XAResource.class};
                            Method registerXAResource_Method = com_sun_jbi_framework_ComponentContext_Class.getMethod("registerXAResource", parameterTypes);                                
                            Object[] arguments = new Object[] {xar};
                            registerXAResource_Method.invoke(componentContext, arguments);
                            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                                mLogger.log(LogSupport.LEVEL_DEBUG,
                                            "JMSXARecovery_RECOVERY_REGISTERED_XARESOURCE",
                                            new Object[]{rec.getConnectionURL()});
                            }
                            recsRecovered.add(rec);
                        } catch (Throwable t) {
                            mLogger.log(Level.WARNING,
                                        mMessages.getString("JMSBC-W0504.RegisterXAResourceFailed",
                                          new Object[] {rec.getConnectionURL()}),
                                        t);
                    		    AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0504.RegisterXAResourceFailed",
                                          new Object[] {rec.getConnectionURL()}), 
                                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                    null, 
                                    AlertsUtil.getServerType(),
                                    AlertsUtil.COMPONENT_TYPE_BINDING,
                                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                    NotificationEvent.EVENT_TYPE_ALERT,
                                    "JMSBC-W0504");                                         
                            continue;
                        }
                    }
                    persister.remove((ConnectionInfoRecord [])recsRecovered.toArray(new ConnectionInfoRecord[recsRecovered.size()]));
                }
            } catch (Throwable t) {
                String errMsg = mMessages.getString(
                            "JMSBC-E0506.XARecoveryUnexpectedError");
                throw new JMSXARecoveryException(errMsg, t);            
            }
        } else {
            mLogger.log(Level.WARNING,
                        "JMSBC-W0503.XARecoveryNotSupported");
           AlertsUtil.getAlerter().warning (mMessages.getString("JMSBC-W0503.XARecoveryNotSupported"), 
		    		             AlertsUtil.SUN_JMS_BINDING, 
		                         null, 
		                         AlertsUtil.getServerType(),
		                         AlertsUtil.COMPONENT_TYPE_BINDING,
		                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
		                         NotificationEvent.EVENT_TYPE_ALERT,
		                         "JMSBC-W0503");
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
            mLogger.log(Level.WARNING,
                    mMessages.getString("JMSBC-W0505.DeterminingXARecoverySupportFailed",
                       new Object [] {t.getLocalizedMessage()}),
                    t);
            AlertsUtil.getAlerter().warning (mMessages.getString("JMSBC-W0505.DeterminingXARecoverySupportFailed",
                       new Object [] {t.getLocalizedMessage()}), 
		    		             AlertsUtil.SUN_JMS_BINDING, 
		                         null, 
		                         AlertsUtil.getServerType(),
		                         AlertsUtil.COMPONENT_TYPE_BINDING,
		                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
		                         NotificationEvent.EVENT_TYPE_ALERT,
		                         "JMSBC-W0505");
            ret = false;
        }
        return ret;
    }
}
