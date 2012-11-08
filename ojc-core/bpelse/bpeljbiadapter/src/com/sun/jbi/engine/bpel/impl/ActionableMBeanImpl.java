/* *************************************************************************
 *
 *          Copyright (c) 2002, Sun Microsystems, Inc.
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems, Inc.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems, Inc.
 *
 ***************************************************************************/
package com.sun.jbi.engine.bpel.impl;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Collection;
import java.util.Locale;

import javax.management.NotCompliantMBeanException;
import javax.management.StandardMBean;
import javax.xml.transform.stream.StreamSource;

import com.sun.jbi.engine.bpel.ActionableMBean;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.persist.PurgeData;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;

/**
 * @author Sun Inc
 * Dec 4, 2007
 */
public class ActionableMBeanImpl extends StandardMBean implements ActionableMBean {

    private Engine mEng;
    private String mActionsXML;
    
    private final static String ACTIONS_FILE = "messages/actions";
    private final static String XML_SUFFIX = ".xml";

    private final static String PURGE_PERSIST_TOKEN = "$EblPrstPrg$";
    private final static String PURGE_MONITOR_TOKEN = "$EblMtrPrg$";
    private final static String ARCHIVE_MONITOR_TOKEN = "$EblMtrArc$";
    
    /**
     * @param mbeanInterface
     * @throws NotCompliantMBeanException
     */
    public ActionableMBeanImpl(Engine engine)
            throws NotCompliantMBeanException, Exception {
        super(ActionableMBean.class);
        mEng = engine;
        InputStream ipStream = null;
        try {
            String fileName = ACTIONS_FILE;
            Locale locale = Locale.getDefault();
            String localeSuffix = locale.toString();
            if (localeSuffix.length() > 0) {
                fileName += "_" + localeSuffix;
            }
            fileName += XML_SUFFIX;
            
            URL fileURL = getClass().getResource(fileName);
            if (fileURL == null) {
                fileName = ACTIONS_FILE + XML_SUFFIX;
                fileURL = getClass().getResource(fileName);
            }
            ipStream = fileURL.openStream();
            
            StreamSource source = new StreamSource(ipStream);
            mActionsXML = DOMHelper.createXmlString(source);

        } catch (Exception e) {
            throw e;
        } finally {
            if (ipStream != null) {
                try {
                    ipStream.close();
                } catch (IOException ex) {
                   throw ex;
                }
            }
        }
    }

    /** @see com.sun.jbi.engine.bpel.ActionableMBean#getActions()
     */
    public String getActions() {
        String retVal;
        if (mEng.isPersistenceEnabled()) {
            retVal = mActionsXML.replace(PURGE_PERSIST_TOKEN, "true");
        } else {
            retVal = mActionsXML.replace(PURGE_PERSIST_TOKEN, "false");
        }
        if (mEng.isMonitorEnabled()) {
            retVal = retVal.replace(PURGE_MONITOR_TOKEN, "true");
            retVal = retVal.replace(ARCHIVE_MONITOR_TOKEN, "true");                        
        } else {
            retVal = retVal.replace(PURGE_MONITOR_TOKEN, "false");
            retVal = retVal.replace(ARCHIVE_MONITOR_TOKEN, "false");
        }
        return retVal;
    }
    
    /** @see com.sun.jbi.engine.bpel.ActionableMBean#purgePersistenceData()
     */
    public void purgePersistenceData() throws Exception {
        DBConnectionFactory dbConnFac = mEng.getDBConnectionFactory();
        PurgeData purge = new PurgeData();
        purge.purgePersistenceData(dbConnFac);
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.ActionableMBean#purgeMonitorData()
     */
    public void purgeMonitorData() throws Exception {
        DBConnectionFactory dbConnFac = mEng.getDBConnectionFactory();
        PurgeData purge = new PurgeData();
        purge.purgeMonitoringData(dbConnFac);
        // if there is no exception from the DB operation, reset the 
        // instances count on the performance manager instances.
        Collection<BPELProcessManager> coll = mEng.getBPELProcessManagers();
        for (BPELProcessManager mgr : coll) {
            mgr.getPerformanceManger().purgeInstanceCounts();
        }
    }
    
}
