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
 * @(#)FileComponentContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.filebc.management.FileBCManagementMBean;

import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;

/**
 * singleton used to share the component context within the File BC
 */
public class FileComponentContext {

    public static final String CRMP_GROUP_ID = "com.sun.jbi.messaging.groupid";
    public static final String CRMP_MESSAGE_ID = "com.sun.jbi.messaging.messageid";
    private static FileComponentContext instance = new FileComponentContext();
    private ComponentContext context;
    private MessagingChannel channel;
    private ComponentLifeCycle lifeCycle;
    private FileBCManagementMBean mManagementMBean;

    /** Creates a new instance of FileComponentContext */
    private FileComponentContext() {
    }

    public static FileComponentContext getInstance() {
        return instance;
    }

    public ComponentContext getContext() {
        return context;
    }

    public void setContext(ComponentContext context) {
        this.context = context;
    }

    /**
     * @return the component lifecycle associated with this context
     * if it has been initialized
     */
    public ComponentLifeCycle getAssociatedLifeCycle() {
        return lifeCycle;
    }

    /**
     * Set the component lifecycle associated with this context
     */
    public void setAssociatedLifeCycle(ComponentLifeCycle aLifeCycle) {
        lifeCycle = aLifeCycle;
    }

    /**
     * @return Obtain the channel associated with this context
     * if it has been initialized
     */
    public MessagingChannel getBindingChannel() {
        return channel;
    }

    /**
     * Set the initizalied channel associated with this context
     */
    public void setBindingChannel(MessagingChannel aChannel) {
        channel = aChannel;
    }

    public FileBCManagementMBean getManagementMBean() {
        return mManagementMBean;
    }

    public void setManagementMBean(FileBCManagementMBean managementMBean) {
        mManagementMBean = managementMBean;
    }
}
