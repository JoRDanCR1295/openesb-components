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
 * @(#)ServerEventListener.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.services.administration.providers;

/**
 * This is a listener interface for getting notifications on standalone instance
 * addition/removal.
 *
 * @author graj
 *
 */
public interface ServerEventListener {
    /**
     * Notification on the creation of a non-clustered instance
     *
     * @param instanceName - name of the standalone instance.
     */
    void createdStandaloneInstance(String instanceName)
        throws Exception;
    
    /**
     * Notification on the deletion of a non-clustered instance
     *
     * @param instanceName - name of the standalone instance.
     */
    void deletedStandaloneInstance(String instanceName)
        throws Exception;
    
    /**
     * Notification on the creation of a cluster
     *
     * @param clusterName - name of the cluster
     */
    void createdCluster(String clusterName)
        throws Exception;
    
    /**
     * Notification on the deletion of a cluster
     *
     * @param clusterName - name of the cluster
     */
    void deletedCluster(String clusterName)
        throws Exception;

}
