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
 * @(#)XslCache.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.util;

import java.util.Collection;
import java.util.Set;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;
import com.sun.jbi.common.util.AbstractPool;
import com.sun.jbi.common.util.EntryRegistry;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.transform.api.Xslt2Support.CompiledXslt2;

/**
 * 
 * @author Kevan Simpson
 */
public class XslCache implements NotificationListener {
    private EntryRegistry<String, Object> mCache;
    
    public XslCache() {
        mCache = new EntryRegistry<String, Object>();
    }
    
    /** @see javax.management.NotificationListener#handleNotification(javax.management.Notification, java.lang.Object) */
    public void handleNotification(Notification notification, Object handback) {
        if (notification instanceof AttributeChangeNotification) {
            AttributeChangeNotification attrNotif = (AttributeChangeNotification) notification;
            String attrName = attrNotif.getAttributeName();
            if (Engine.THREAD_COUNT.equals(attrName)) {
                int threadCount = Util.parseInt(
                        String.valueOf(attrNotif.getNewValue()), -1);
                if (threadCount > 0) {
                    for (Object xsl : stylesheets()) {
                        if (xsl instanceof AbstractPool) {
                            // 1 pooled xsl per nmr-polling thread (i.e. 1:1)
                            ((AbstractPool) xsl).resizePool(threadCount);
                        }
                    }
                }
                // else nothing... BPELSE goes to default count if value < 1
            }
        }
    }

    public void addStylesheet(String location, Object xsl) {
        if (!Util.isEmpty(location)) {
            mCache.register(location, xsl);
        }
    }
    
    public void cleanup() {
        for (String key : mCache.keySet()) {
            Object obj = mCache.remove(key);
            if (obj instanceof CompiledXslt2) {
                ((CompiledXslt2) obj).cleanup();
            }
        }
    }
    
    public Object getStylesheet(String location) {
        return mCache.lookup(location);
    }
    
    public Set<String> locations() {
        return mCache.keySet();
    }
    
    public Collection<Object> stylesheets() {
        return mCache.entries();
    }
}
