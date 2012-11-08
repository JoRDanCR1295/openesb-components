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
 * @(#)$Id: FTPBCManagement.java,v 1.2 2010/02/04 02:59:48 fkieviet Exp $                                     
 *                                                                                                                
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.                                                
 *                                                                                                                
 * END_HEADER - DO NOT EDIT                                                                                       
 */
package com.sun.jbi.ftpbc.management;

import com.sun.jbi.ftpbc.FTPBindingLifeCycle;
import com.sun.jbi.internationalization.Messages;

import java.util.logging.Logger;
import javax.management.MBeanException;

/**                                                                                                               
 * The Management MBean implementation to suspend and resume                                                                     
 * endpoints                                                                                                      
 *                                                                                                                
 */
public class FTPBCManagement implements FTPBCManagementMBean {

    private static final Messages mMessages =
            Messages.getMessages(FTPBCManagement.class);
    private FTPBindingLifeCycle mLifecycleListener;
    private Logger mLogger = Messages.getLogger(FTPBCManagement.class);

    public FTPBCManagement(FTPBindingLifeCycle lifecycleListener) {
        this.mLifecycleListener = lifecycleListener;
    }

    /** Suspend a consuming endpoint                                                                                  
     *                                                                                                           
     *@param  endpointName an unique consuming endpoint identifier                                                  
     *@throws MBeanException on error                                                                            
     */
    public boolean suspend(String consumingEndpointName) throws MBeanException {
        boolean isSuspended = false;
        if (consumingEndpointName == null || consumingEndpointName.equals("")) {
            throw new MBeanException(new Exception(mMessages.getString("FTPBC-E001049.Invalid_endpoint_name", consumingEndpointName)));
        }
        try {
            isSuspended = mLifecycleListener.suspendActivatedEndpoint(consumingEndpointName);
        } catch (Exception e) {
            throw new MBeanException(e, mMessages.getString("FTPBC-E001050.Failed_to_suspend_endpoint", consumingEndpointName) + e.getMessage());
        }

        return isSuspended;

    }

    /** Resume a consuming endpoint                                                                                       
     *                                                                                                           
     * @param  endpointName an unique consuming endpoint identifier                                      
     * *@throws MBeanException on error                                                                          
     */
    public boolean resume(String consumingEndpointName) throws MBeanException {
        boolean isResumed = false;
        if (consumingEndpointName == null || consumingEndpointName.equals("")) {
            throw new MBeanException(new Exception(mMessages.getString("FTPBC_E001049.Invalid_endpoint_name", consumingEndpointName)));
        }

        try {
            isResumed = mLifecycleListener.resumeActivatedEndpoint(consumingEndpointName);
        } catch (Exception e) {
            throw new MBeanException(e, mMessages.getString("FTPBC-E001051.Failed_to_resume_endpoint", consumingEndpointName) + e.getMessage());
        }

        return isResumed;
    }

    /** Returns true if an consuming endpoint is active,
     * or false if it is suspended
     * No-op for provisioning endpoints.
     *
     * @param endpointName an unique consuming endpoint identifier
     * @return true if endpoint is active, false if it is suspended
     */
    public boolean isEndpointActive(String consumingEndpointName) throws MBeanException {
        if (consumingEndpointName == null || consumingEndpointName.equals("")) {
            throw new MBeanException(new Exception(mMessages.getString("FTPBC-E001049.Invalid_endpoint_name", consumingEndpointName)));
        }

        return mLifecycleListener.isEndpointActive(consumingEndpointName);
    }

    /** Returns an array of unique endpoint names for active
     * consuming endpoints.
     *
     * @return an array of unique endpoint names for active consuming endpoints
     */
    public String[] listActiveEndpoints() {
        return mLifecycleListener.getActiveConsumingEndpoints();
    }

    /** Returns an array of unique endpoint names for inactive (suspended)
     * consuming endpoints.
     *
     * @return an array of unique endpoint names for inactive(suspended) consuming endpoints
     */
    public String[] listInactiveEndpoints() {
        return mLifecycleListener.getInactiveConsumingEndpoints();
    }
}    
                                                                                                                                                         