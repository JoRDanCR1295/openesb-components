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
 * @(#)Notifier.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator;

import java.sql.Connection;
import java.sql.Timestamp;

/*
 * RelationNotifier.java
 *
 * Marker interface
 *
 * Created on July 26, 2005, 4:07 PM
 *
 * @author Bing Lu
 */
import java.util.List;

public interface Notifier {
    int EVENT_TYPE_OBJECT_ARRAY = 0;
    int EVENT_TYPE_MAP = 1;
    int EVENT_TYPE_XML_DOCUMENT = 2;

    public void setEventType(int deliveryType);

    public boolean includeTimestamp();
    
    public int getCacheSize();
    
    public void fetch(Connection con, Timestamp timestamp);

    /**
     * Get a Notification whose timestamp happened before argument timestamp
     * It will return a different Notification object only the previous returned
     * one is removed through removeNotification
     * @param con
     * @param timestamp
     * @return a Notification whose timestamp happened before argument timestamp
     */
    public Notification getNotification();
    
    /**
     * Remove the argument Notification 
     * @param con
     * @param n
     */
    public void removeNotification(Connection con, Notification n) throws Exception;
    
    /**
     * Get a batch of Notifications whose timestamps happened before argument timestamp, and
     * the batch size is no greater than the argument maxBatchSize
     * It will return a different batch of Notification objects only if the previous returned
     * one is removed through removeNotificationBatch
     * 
     * @param con
     * @param timestamp
     * @param maxBatchSize
     * @return
     */
    public List<Notification> getNotificationBatch(int maxBatchSize);
    
    /**
     * Remove the argument batch of Notifications
     * @param con
     * @param n
     */
    public void removeNotificationBatch(Connection con, List<Notification> batch) throws Exception;
}
