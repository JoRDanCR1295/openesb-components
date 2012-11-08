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
 * @(#)$Id: FileBCManagement.java,v 1.2 2009/08/28 21:47:19 jfu Exp $                                     
 *                                                                                                                
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.                                                
 *                                                                                                                
 * END_HEADER - DO NOT EDIT                                                                                       
 */
package com.sun.jbi.filebc.management;

import java.util.Set;
import java.util.logging.Logger;

import javax.management.MBeanException;

import com.sun.jbi.filebc.FileBindingLifeCycle;
import com.sun.jbi.internationalization.Messages;

/**                                                                                                               
 * The Management MBean implementation to suspend and resume                                                                     
 * endpoints.
 *                                                                                                                
 */
public class FileBCManagement implements FileBCManagementMBean {

    private static final Messages mMessages =
            Messages.getMessages(FileBCManagement.class);
    private FileBindingLifeCycle mLifecycleListener;
    private Logger mLogger = Messages.getLogger(FileBCManagement.class);

    public FileBCManagement(FileBindingLifeCycle lifecycleListener) {
        this.mLifecycleListener = lifecycleListener;
    }

    /** Suspend a consuming(inbound) endpoint                                                                                  
     *                                                                                                           
     *@param  endpointName an unique consuming(inbound) endpoint identifier                                                  
     *@throws MBeanException on error                                                                            
     */
    public boolean suspend(String consumingEndpointName) throws MBeanException {
        boolean isSuspended = false;
        if (consumingEndpointName == null || consumingEndpointName.equals("")) {
            throw new MBeanException(new Exception(mMessages.getString("FILEBC-E01300.Invalid_endpoint_name", consumingEndpointName)));
        }
        try {
            isSuspended = mLifecycleListener.suspendInboundEndpoint(consumingEndpointName);
        } catch (Exception e) {
            throw new MBeanException(e, mMessages.getString("FILEBC-E01301.Failed_to_suspend_endpoint", consumingEndpointName) + e.getMessage());
        }

        return isSuspended;

    }

    /** Resume a consuming(inbound) endpoint                                                                                       
     *                                                                                                           
     * @param  endpointName an unique consuming(inbound) endpoint identifier                                      
     * *@throws MBeanException on error                                                                          
     */
    public boolean resume(String consumingEndpointName) throws MBeanException {
        boolean isResumed = false;
        if (consumingEndpointName == null || consumingEndpointName.equals("")) {
            throw new MBeanException(new Exception(mMessages.getString("FILEBC-E01300.Invalid_endpoint_name", consumingEndpointName)));
        }

        try {
            isResumed = mLifecycleListener.resumeInboundEndpoint(consumingEndpointName);
        } catch (Exception e) {
            throw new MBeanException(e, mMessages.getString("FILEBC-E01302.Failed_to_resume_endpoint", consumingEndpointName) + e.getMessage());
        }

        return isResumed;
    }

    /** Returns true if an consuming(inbound) endpoint is active,
     * or false if it is suspended
     * No-op for provisioning endpoints.
     *
     * @param endpointName an unique consuming(inbound) endpoint identifier
     * @return true if endpoint is active, false if it is suspended
     */
    public boolean isEndpointActive(String consumingEndpointName) throws MBeanException {
        if (consumingEndpointName == null || consumingEndpointName.equals("")) {
            throw new MBeanException(new Exception(mMessages.getString("FILEBC-E01300.Invalid_endpoint_name", consumingEndpointName)));
        }

        return mLifecycleListener.isInboundEndpointActive(consumingEndpointName);
    }

    /** Returns an array of unique endpoint names for active
     * consuming(inbound) endpoints.
     *
     * @return an array of unique endpoint names for active consuming(inbound) endpoints
     */
    public String[] listActiveEndpoints() {
        Set<String> set = mLifecycleListener.getActiveInboundEndpointNames();
        String[] arr = new String[0];
        if (set != null && set.size() > 0) {
            arr = set.toArray(arr);
        }
        return arr;
    }

    /** Returns an array of unique endpoint names for inactive (suspended)
     * consuming(inbound) endpoints.
     *
     * @return an array of unique endpoint names for inactive(suspended) consuming(inbound) endpoints
     */
    public String[] listInactiveEndpoints() {
        Set<String> set = mLifecycleListener.getInActiveInboundEndpointNames();
        String[] arr = new String[0];
        if (set != null && set.size() > 0) {
            arr = set.toArray(arr);
        }
        return arr;
    }
}    
                                                                                                                                                         