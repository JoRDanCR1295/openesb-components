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
 * @(#)PreferencesBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.preferences;
import java.util.logging.Logger;
/**
 *
 * @author ylee
 */
public class PreferencesBean {
    
    private String refreshInterval = "30";      // seconds
    private static Logger logger = Logger.getLogger(PreferencesBean.class.getName());
    private boolean autoRefresh = false;
    
    private Preferences prefs;
    
    /** Creates a new instance of Preferences */
    public PreferencesBean() {
        prefs = PreferencesFactory.getPreferencesInstance();
        refreshInterval = prefs.getRefreshInterval();
        autoRefresh = prefs.getAutoRefresh();
    }
    
    public void setRefreshInterval(String value) {
        refreshInterval = value;
    }
    
    public String getRefreshInterval() {
        return refreshInterval;
    }
    
    public void setAutoRefresh(boolean value) {
        autoRefresh = value;
    }
    
    public boolean getAutoRefresh() {
        return autoRefresh;
    }
    
    public void save() {
        // save preferences...
        logger.info("save...");
        prefs.setRefreshInterval(refreshInterval);
        prefs.setAutoRefresh(autoRefresh);
        prefs.save();
        
    }
    
}
