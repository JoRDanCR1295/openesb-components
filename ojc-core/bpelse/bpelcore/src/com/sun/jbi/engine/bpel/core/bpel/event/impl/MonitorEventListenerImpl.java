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
 * @(#)$Id: MonitorEventListenerImpl.java,v 1.6 2009/04/01 01:47:29 vinayram Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.event.impl;

import java.util.Hashtable;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEventListener;
import com.sun.jbi.engine.bpel.core.bpel.util.EventProcessHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;

public class MonitorEventListenerImpl implements BPELEventListener {

    private boolean mDoProcess = false;

    private DBConnectionFactory mdbFactory;

    private static final Logger LOGGER = Logger.getLogger(MonitorEventListenerImpl.class.getName());

    private Hashtable<Thread, BPELEventPersister> mPersisters = new Hashtable<Thread, BPELEventPersister>();

    public void processEvent(BPELEvent event) {
        if (mDoProcess) {
            Thread th = Thread.currentThread();
            BPELEventPersister evPersister = mPersisters.get(th);
            boolean isFine = true;
            if (evPersister == null) {
                try {
                    evPersister = new BPELEventPersister(mdbFactory);
                } catch (Exception e) {
                    isFine = false;
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6163: SQLException in init connection and " +
                    		"Prepared Statement"), e);
                }
                if (isFine) {
                    mPersisters.put(th, evPersister);    
                }
            }
            evPersister.persistEvent(event);
        }
    }

    public void init(EventProcessHelper eventHelper) {
        if (eventHelper.getEngine().isMonitorEnabled()) {
            mDoProcess = true;
//            mDataSource = eventHelper.getDataSource();
            mdbFactory = eventHelper.getFactory();
            
        }
    }

    public void resetProperties(Properties properties) {
        String monitorEnabled = properties.getProperty(Engine.MONITOR_ENABLED);
        if (monitorEnabled != null && monitorEnabled.equalsIgnoreCase("true")) {
            mDoProcess = true;
        } else if (monitorEnabled != null && !monitorEnabled.equalsIgnoreCase("true")) {
            mDoProcess = false;
            mPersisters.clear();
        }
    }

    public void shutdown() {
    	mPersisters.clear();
    }

}
