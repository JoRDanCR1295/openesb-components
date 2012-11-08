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
 * @(#)SMTPBindingDeployer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;

/**
 *
 * @author ckuo
 */
public class SMTPBindingDeployer implements ServiceUnitManager, EndpointChangeSubject {
    private static final Messages mMessages =
        Messages.getMessages(SMTPBindingDeployer.class);

    private static final Logger mLogger =
        Messages.getLogger(SMTPBindingDeployer.class);
    
    private HashMap mServiceUnits;
    private ComponentContext mContext;
    private StatusProviderHelper mStatusProviderHelper;
    private Collection mEndpointChangeListeners;
    private SMTPBindingComponent mSMTPBindingComponent;

    /**
     * 
     *
     * @param        context 
     */
    public SMTPBindingDeployer(final ComponentContext context,
                               final SMTPBindingComponent smtpBindingComponent) {
        mContext = context;
        mStatusProviderHelper = smtpBindingComponent.getStatusProviderHelper();
        mServiceUnits = new HashMap();
        mEndpointChangeListeners = new HashSet();
        mSMTPBindingComponent = smtpBindingComponent;
    }
    
    ////////
    //
    //  ServiceUnitManager Interface Methods
    //
    ////////

    public String deploy(final String suId,
                         final String asaFilePath)
        throws DeploymentException {

        String retMsg = null;
        final String taskName = "deploy";
        
        if (SMTPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingDeployer.mLogger.log(Level.INFO,
                        "SMTPBD_Deploy_SU",
                        new Object[]{suId, asaFilePath});
        }
        ServiceUnit su = null;
        
        su = (ServiceUnit)mServiceUnits.get(suId);
        
        if(su == null){
            su = new ServiceUnitImpl(suId, asaFilePath,mContext,
                    mStatusProviderHelper,
                    mEndpointChangeListeners,mSMTPBindingComponent.getRuntimeConfig());
        }
        try {
			su.deploy(asaFilePath);
		} catch (Exception ex) {
            if (su != null) {
                try {
                    su.stop();
                } catch (Throwable th) {
                    // Ignore on purpose.
                }
                try {
					su.shutdown();
				} catch (Throwable th) {
					// Ignore on purpose	
				}
            }
            String errMsg = mMessages.getString("SMTPBD_Failed_deploy_SU", ex.getMessage());
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "SMTPBC_DEPLOY_1",
                        null,
                        errMsg, 
                        ex);
                throw new DeploymentException(exMsg, ex); 
            
		}
		
        //Put the service unit map, only after the deployment succeeds
        mServiceUnits.put(suId, su);
		
        retMsg = createSuccessMessage(taskName, mContext.getComponentName());
        
        return retMsg;
    }
    
    public void init(final String suId, final String suPath) throws DeploymentException {
        final String taskName = "init";
        if (SMTPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingDeployer.mLogger.log(Level.INFO,
                        "SMTPBD_Init_SU",
                        new Object[] {suId, suPath});
        }
        
        ServiceUnit su = null;
        try {
        	su = (ServiceUnit)mServiceUnits.get(suId);
            if (su == null) {
                su = new ServiceUnitImpl(suId, suPath,mContext,
                                                     mStatusProviderHelper,
                                                     mEndpointChangeListeners,mSMTPBindingComponent.getRuntimeConfig());
            }
            su.init(suPath);
            mServiceUnits.put(suId, su);
            
            if (SMTPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
                SMTPBindingDeployer.mLogger.log(Level.INFO, "SMTPBD_Complete_init_SU", suId);
            }                
            
        } catch (final Exception ex) {
            // Clean up our state
            if (su != null) {
                try {
                    su.shutdown();
                } catch (Throwable th) {
                    // Ignore on purpose.
                }
            }        	
            final String errMsg = SMTPBindingDeployer.mMessages.getString("SMTPBD_Failed_init_SU", ex.getMessage());
            SMTPBindingDeployer.mLogger.log(Level.SEVERE, errMsg);
            final String exMsg = createExceptionMessage(mContext.getComponentName(),
                                                  taskName,
                                                  "FAILED",
                                                  "FBC_PROCESS_2",
                                                  suId,
                                                  "Processing deployment error: "
                                                      + ex.getMessage(),
                                                  ex);
            throw new DeploymentException(exMsg);

        }
    }
    
    
    public void shutDown(final String suId) throws DeploymentException {
        final String taskName = "shutDown";        
        if (SMTPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingDeployer.mLogger.log(Level.INFO, "SMTPBD_Shutdown_SU", suId);
        }        
        final ServiceUnit su = (ServiceUnit)mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.shutdown();
            } catch (final Exception  ex) {
            	final String errMsg = SMTPBindingDeployer.mMessages.getString("SMTPBD_Error_shutdown_SU", ex.getMessage());
            	SMTPBindingDeployer.mLogger.log(Level.SEVERE, errMsg);                
                final String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "SMTPBC_STOP_1",
                        suId,
                        "SMTP BC stop error " + ex.getMessage() , ex);
                throw new DeploymentException(exMsg, ex);
            }
        }
    }
    
    public void start(final String suId) throws DeploymentException {
        final String taskName = "start";
        if (SMTPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingDeployer.mLogger.log(Level.INFO, "SMTPBD_Starting_SU", suId);
        }        
        final ServiceUnit su = (ServiceUnit)mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.start();
            } catch (final Exception  ex) {
            	final String errMsg = SMTPBindingDeployer.mMessages.getString("SMTPBD_Error_start_SU", ex.getMessage());
            	SMTPBindingDeployer.mLogger.log(Level.SEVERE, errMsg);                
                final String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "SMTPBC_START_1",
                        suId,
                        "File BC stop error " + ex.getMessage() , ex);
                throw new DeploymentException(exMsg, ex);
            }
        }
        if (SMTPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingDeployer.mLogger.log(Level.INFO, "SMTPBD_Complete_start_BC");
        }
    }
    
    public void stop(final String suId) throws DeploymentException {
        final String taskName = "stop";
        if (SMTPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingDeployer.mLogger.log(Level.INFO, "SMTPBD_Stop_SU", suId);
        }        
        final ServiceUnit su = (ServiceUnit)mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.stop();
            } catch (final Exception  ex) {
            	final String errMsg = SMTPBindingDeployer.mMessages.getString("SMTPBD_Error_stop_SU", ex.getMessage());
            	SMTPBindingDeployer.mLogger.log(Level.SEVERE, errMsg);                
                final String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "SMTPBC_START_1",
                        suId,
                        "SMTP BC stop error " + ex.getMessage() , ex);
                throw new DeploymentException(exMsg, ex);
            }
        }
        if (SMTPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingDeployer.mLogger.log(Level.INFO, "SMTPBD_Complete_stop_SU", suId);
        }
    }
    
    
    /**
     * Cancel a Service Deployment.  If the deployment is in use
     * (has dependencies), then this operation may fail.
     *
     * @param name - name of the service unit
     * @param root - root of the service unit
     */
    public String undeploy(final String name, final String root)
        throws DeploymentException {

        String retMsg = null;
        final String taskName = "undeploy";
        
        final ServiceUnit su = (ServiceUnit)mServiceUnits.get(name);
        if (su != null) {
            mServiceUnits.remove(name);
        }

        if (SMTPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            SMTPBindingDeployer.mLogger.log(Level.INFO, "SMTPBD_Undeploy_SU", new Object[]{name, root});
        }

        retMsg = createSuccessMessage(taskName, mContext.getComponentName());
        return retMsg;
    }
    
    
    ////////
    //
    //  EndpointChangeSubject Interface Methods
    //
    ////////

    public void addEndpointChangeListener(final EndpointChangeListener listener) {
        mEndpointChangeListeners.add(listener);
//         Iterator<ServiceUnit> serviceUnits = mServiceUnits.values().iterator();
//         while (serviceUnits.hasNext()) {
//             EndpointChangeSubject subject = (EndpointChangeSubject)serviceUnits.next();
//             subject.addEndpointChangeListener(listener);
//         }
    }

    public void addEndpointChangeListener(final Collection listeners) {
        mEndpointChangeListeners.addAll(listeners);
//         Iterator<ServiceUnit> serviceUnits = mServiceUnits.values().iterator();
//         while (serviceUnits.hasNext()) {
//             EndpointChangeSubject subject = (EndpointChangeSubject)serviceUnits.next();
//             subject.addEndpointChangeListener(listeners);
//         }
    }

    public void removeEndpointChangeListener(final EndpointChangeListener listener) {
        mEndpointChangeListeners.remove(listener);
//         Iterator<ServiceUnit> serviceUnits = mServiceUnits.values().iterator();
//         while (serviceUnits.hasNext()) {
//             EndpointChangeSubject subject = (EndpointChangeSubject)serviceUnits.next();
//             subject.removeEndpointChangeListener(listener);
//         }
    }


    ////////
    //
    //  SMTPBindingDeployer Public Methods
    //
    ////////

    public Collection getServiceUnits() {
        return Collections.unmodifiableCollection(mServiceUnits.values());
    }

    ////////
    //
    //  SMTPBindingDeployer Private Methods
    //
    ////////
    
    private String createSuccessMessage(final String taskName, final String componentName) {

        final JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        final String retMsg = msgBuilder.createSuccessMessage(taskName);
        return retMsg;
    }
    
    private String createExceptionMessage(final String componentName,
                                          final String taskName,
                                          final String status,
                                          final String locToken,
                                          final String locParam,
                                          final String locMessage,
                                          final Throwable exObj) {

        final JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        final String retMsg = msgBuilder.createExceptionMessage(taskName, locToken, locMessage, locParam, exObj);
        return retMsg;
    }
    
    
}
