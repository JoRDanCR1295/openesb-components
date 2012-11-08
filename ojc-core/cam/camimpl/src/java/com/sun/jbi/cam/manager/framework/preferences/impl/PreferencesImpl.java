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
 * @(#)PreferencesImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.preferences.impl;

import com.sun.jbi.cam.manager.framework.preferences.Preferences;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;

/**
 *
 * @author ylee
 */
public class PreferencesImpl implements Preferences {
    
    private static String CONFIG_PROPERTIES_FILENAME = "cam.properties";
    // key names
    private static String REFRESH_INTERVAL = "refreshInterval";
    private static String AUTO_REFRESH = "autoRefresh";
    
    private Properties prefs = null;
    private String refreshInterval = "30";      
    private boolean autoRefresh = false;
    
    private static Preferences instance = null;
    
    
    public static Preferences getInstance() {
        if ( instance==null ) {
            instance = new PreferencesImpl();
        }
        return instance;
    }
    
    /** Creates a new instance of ConfigurationImpl */
    protected PreferencesImpl() {
        prefs = loadPreferencesData();
    }

    public String getRefreshInterval() {
        return prefs.getProperty(REFRESH_INTERVAL);
    }
    
    public void setRefreshInterval(String interval) {
        this.refreshInterval = interval;
    }
    
    public void setAutoRefresh(String refresh) {
        setAutoRefresh(Boolean.getBoolean(refresh));
    }
    
    public void setAutoRefresh(boolean refresh) {
        autoRefresh = refresh;
    }
    
    public boolean getAutoRefresh() {
        return autoRefresh;
    }
    
    public void save() {
        // save configuration
        Properties props = new Properties();
        props.put(REFRESH_INTERVAL,refreshInterval);
        props.put(AUTO_REFRESH,Boolean.toString(autoRefresh));
        savePreferencesData(props);        
    }
    
    
    /** save preferences data to DB -
     *  current implementation - save to a properties file
     */
    private void savePreferencesData(Properties props) {

        File f = new File(getPreferencesFileName());
        FileOutputStream fos = null;
        try {
            f.createNewFile();
            fos = new FileOutputStream(f);
            props.store(fos,null);
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if ( fos!=null ) {
                try {
                    fos.close();
                } catch (IOException e1) {
                    e1.printStackTrace();
                }
            }
        }
    }
    
    
    public void load() {
        // load configuration
        prefs = loadPreferencesData();
        refreshInterval = prefs.getProperty(REFRESH_INTERVAL);
        autoRefresh = getBooleanProperty(AUTO_REFRESH,prefs);
    }
    
    
    private boolean getBooleanProperty(String key,Properties props) {
        boolean bool = false;
        String value = props.getProperty(key);
        if ( value!=null ) {
            try {
                bool = Boolean.getBoolean(value);
            } catch(Exception e) {
                //
            }
        }
         return bool;
    }
    
    private Properties loadDefaults() {
        Properties props = new Properties();
        props.setProperty(REFRESH_INTERVAL,"30");                    // default - 
        props.setProperty(AUTO_REFRESH,Boolean.TRUE.toString());     // default
        return props;
    }
    
    
    private String getPreferencesFileName() {
        // @CAVEAT use Glassfish instance root - this need to be refactored if CAM is hosted by other AS
        String instanceRoot = System.getProperty("com.sun.aas.instanceRoot");
        StringBuffer fileName = new StringBuffer(instanceRoot);
        fileName.append(File.separator + "config" + File.separator + CONFIG_PROPERTIES_FILENAME);        
        return fileName.toString();
    }
    
    
    private Properties loadPreferencesData() {
    	String filename = getPreferencesFileName();
        Properties props = new Properties();
        FileInputStream fis = null;
        try {
            fis = new FileInputStream(filename);
            if (fis != null) {
                try {
                    props.load(fis);
                } catch (IOException ex) {
                    //System.out.println("unable to read: " + filename);
                } finally {
                    try {
                        if (fis != null) {
                            fis.close();
                        }
                    } catch (IOException e) {
                    }
                }
            }
        } catch (Exception ex) {
            //System.out.println("Could not find: " + filename);
        }

        // check if there is any preferences file or if no properties is specified
        if ( props==null || props.isEmpty() ) {
            props = loadDefaults();
        }
        
        return props;
    }    
    
    
}
