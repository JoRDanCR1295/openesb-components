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
 * @(#)PojoSEConfigurationMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.pojose.jbi;

import javax.management.NotificationEmitter;


/**
 * Defines configurable properties of PojoSE.
 *
 * @author Girish Patil
 */
public interface PojoSEConfigurationMBean extends NotificationEmitter{
    public static final String CORE_THREAD_POOL_SIZE = "CoreThreadPoolSize"; //NOI18N
    public static final String MAX_THREAD_POOL_SIZE = "MaxThreadPoolSize"; //NOI18N
    public static final String THREAD_POOL_BLOCKING_QUEUE_SIZE = "ThreadPoolBlockingQueueSize"; //NOI18N
    
    public static final Integer DEFAULT_CORE_THREAD_POOL_SIZE = 15; 
    public static final Integer DEFAULT_MAX_THREAD_POOL_SIZE = 50; 
    public static final Integer DEFAULT_THREAD_POOL_BLOCKING_QUEUE_SIZE = 50; 

    public Integer getCoreThreadPoolSize();
    public void setCoreThreadPoolSize(Integer pz);

    public Integer getMaxThreadPoolSize();
    public void setMaxThreadPoolSize(Integer pz);
    
    /**
     * Returns ThreadPoolBlockingQueueSize. Note ThreadPoolBlockingQueueSize can
     * only be set during bootstrap process. Hence the setter is in bootstrap
     * config MBean.
     * 
     * @return Integer ThreadPoolBlockingQueueSize
     */    
    public Integer getThreadPoolBlockingQueueSize();
    public void setThreadPoolBlockingQueueSize(Integer cz);
}    
