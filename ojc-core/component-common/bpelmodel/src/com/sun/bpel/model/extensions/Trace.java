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
package com.sun.bpel.model.extensions;

import java.util.Collection;

import com.sun.bpel.xml.common.model.XMLElement;

/*
 * @(#)Trace.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
public interface Trace extends XMLElement {

	/** Tag for this element. */
    public static final String TAG = "trace";
    
    /** */
    public static final String ON_START = "onStart";
    
    /** */
    public static final String ON_COMPLETE = "onComplete";
    
    /**
     * 
     * @param log
     */
    public void addLog(Log log);
    
    /**
     * 
     * @param alert
     */
    public void addAlert(Alert alert);
    
    /**
     * 
     * @return
     */
    public Log getLog(int index);
    
    /**
     * 
     * @return
     */
    public Alert getAlert(int index);
    
    /**
     * 
     * @return
     */
    public Collection<Log> getLogs();
    
    /**
     * 
     * @return
     */
    public Collection<Alert> getAlerts();
    
    /**
     * 
     * @return
     */
    public Collection<Log> getOnStartLogs();
    
    /**
     * 
     * @return
     */
    public Collection<Log> getOnCompleteLogs();
    
    /**
     * 
     * @return
     */
    public Collection<Alert> getOnStartAlerts();
    
    /**
     * 
     * @return
     */
    public Collection<Alert> getOnCompleteAlerts();
    
    /**
     * 
     * @param log
     */
    public boolean removeLog(Log log);
    
    /**
     * 
     * @param alert
     */
    public boolean removeAlert(Alert alert);
    
    /**
     * 
     *
     */
    public void clearLogs();
    
    /**
     * 
     *
     */
    public void clearAlerts();
    
    /**
     * 
     * @return
     */
    public int getLogSize();
    
    /**
     * 
     * @return
     */
    public int getAlertSize();
    
    /**
     * 
     * @return
     */
    public boolean hasLogs();
    
    /**
     * 
     * @return
     */
    public boolean hasOnStartLogs();
    
    /**
     * 
     * @return
     */
    public boolean hasOnCompleteLogs();
    
    /**
     * 
     * @return
     */
    public boolean hasAlerts();
    
    /**
     * 
     * @return
     */
    public boolean hasOnStartAlerts();
    
    /**
     * 
     * @return
     */
    public boolean hasOnCompleteAlerts();
}
