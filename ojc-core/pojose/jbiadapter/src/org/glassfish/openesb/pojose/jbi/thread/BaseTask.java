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
 * @(#)BaseTask.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.jbi.thread;

import java.util.concurrent.atomic.AtomicLong;
import javax.jbi.messaging.MessageExchange;

/**
 *
 * @author gpatil
 */
public abstract class BaseTask implements Runnable{
    static final AtomicLong inMsgTaskCreated = new AtomicLong();
    static final AtomicLong inMsgTaskCompleted = new AtomicLong();
    static final AtomicLong respMsgTaskCreated = new AtomicLong();
    static final AtomicLong respMsgTaskCompleted = new AtomicLong();
    
    BaseTask(){    
    }

    public static long getInMsgTaskCompleted() {
        return inMsgTaskCompleted.get();
    }

    public static long getInMsgTaskCreated() {
        return inMsgTaskCreated.get();
    }

    public static long getRespMsgTaskCompleted() {
        return respMsgTaskCompleted.get();
    }

    public static long getRespMsgTaskCreated() {
        return respMsgTaskCreated.get();
    }

    //*****   Instance methods *****
    public abstract String getPojoServiceClassName();
    public abstract MessageExchange getProvisioningME();
    public abstract MessageExchange getTaskTriggerME();
}
