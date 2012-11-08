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
 * @(#)MQConnectionInfoFilePersister.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.mqbc.recovery;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.mqbc.LogSupport;

/**
 * MQ Connection information persister.
 * This is a simple properties file based implementation; no file locking.
 */
public class MQConnectionInfoFilePersister implements ConnectionInfoPersister {
    private Properties connectionProps = new Properties();
    private Map cache = Collections.synchronizedMap(new HashMap());
    private File persistentFile = null;
    private boolean initialized = false;
    public static final String FILE_PERSISTER_PROPS_DIR = "file.persister.dir";
    public static final String FILE_PERSISTER_PROPS_FILENAME = "file.persister.filename";
    public static final String FILE_PERSISTER_DEFAULT_FILENAME="com.sun.jbi.mqbc.connections";
    
    private static final Messages mMessages =
            Messages.getMessages(MQConnectionInfoFilePersister.class);
    private static final Logger mLogger =
            Messages.getLogger(MQConnectionInfoFilePersister.class);
    
    private static final String FILE_PERSISTER_VALUES_DELIM = "|||";
    
    /** Creates a new instance of JMSConnectionInfoFilePersister */
    public MQConnectionInfoFilePersister() {
    }
    
    synchronized public void initialize(Properties persisterProps)
            throws ConnectionInfoPersistException {
        if (initialized) {
            mLogger.log(Level.SEVERE,
                    "MQConnectionInfoFilePersister_ALREADY_INITIALIZED",
                    new Object[]{persistentFile.getParent(),
                    persistentFile.getName()});
            
            String errMsg = mMessages.getString(
                    "MQConnectionInfoFilePersister_ALREADY_INITIALIZED",
                    new Object[]{persistentFile.getParent(),
                    persistentFile.getName()});
            
            throw new ConnectionInfoPersistException(errMsg);
        }
        
        connectionProps.clear();
        cache.clear();
        String dirStr = null;
        String fileStr = null;
        try {
            dirStr = persisterProps.getProperty(FILE_PERSISTER_PROPS_DIR);
            File dir = new File(dirStr);
            if (!dir.exists() || !dir.isDirectory()) {
                dir.mkdir();
            }
            fileStr = persisterProps.getProperty(FILE_PERSISTER_PROPS_FILENAME);
            fileStr = fileStr == null? FILE_PERSISTER_DEFAULT_FILENAME : fileStr;
            persistentFile = new File(dir, fileStr);
            // If there are existing records, load now
            if (persistentFile.exists()) {
                FileInputStream fis = new FileInputStream(persistentFile);
                connectionProps.load(fis);
                fis.close();
            }
            // Now restore records
            Iterator connKeyIter = connectionProps.keySet().iterator();
            while (connKeyIter.hasNext()) {
                String connValues = (String)connectionProps.get(connKeyIter.next());
                MQConnectionInfoRecord mqConn = new MQConnectionInfoRecord();
                deserializeValues(connValues, mqConn);
                cache.put(mqConn.hashCode(), mqConn);
            }
            initialized = true;
        } catch (Throwable t) {
            initialized = false;
            String stackTr = LogSupport.getStackTraceAsString(t);
            mLogger.log(Level.SEVERE,
                    "MQConnectionInfoFilePersister_INITIALIZE_FAILED",
                    new Object[]{dirStr, fileStr, stackTr});
            
            String errMsg = mMessages.getString(
                    "MQConnectionInfoFilePersister_INITIALIZE_FAILED",
                    new Object[]{dirStr, fileStr, stackTr});
            
            throw new ConnectionInfoPersistException(errMsg, t);
        }
    }
    
    synchronized public void persist(ConnectionInfoRecord [] connInfoRecords)
            throws ConnectionInfoPersistException {
        if (!initialized) {
            mLogger.log(Level.SEVERE,
                    "MQConnectionInfoFilePersister_NOT_INITIALIZED");
            String errMsg = mMessages.getString(
                    "MQConnectionInfoFilePersister_NOT_INITIALIZED");
            throw new ConnectionInfoPersistException(errMsg);
        }
        try {
            boolean recsUpdated = false;
            if (connInfoRecords != null && connInfoRecords.length > 0) {
                for (int i=0; i < connInfoRecords.length; i++) {
                    if (connInfoRecords[i] instanceof MQConnectionInfoRecord &&
                            !cache.containsKey(connInfoRecords[i].hashCode())) {
                        cache.put(connInfoRecords[i].hashCode(),
                                connInfoRecords[i]);
                        connectionProps.put(Integer.toString(connInfoRecords[i].hashCode()),
                                serializeValues((MQConnectionInfoRecord)
                                connInfoRecords[i]));
                        recsUpdated = true;
                    }
                }
                
                if (recsUpdated) {
                    forceDiskWriteRecords();
                }
            }
        } catch (Throwable t) {
            String stackTr = LogSupport.getStackTraceAsString(t);
            mLogger.log(Level.SEVERE,
                    "MQConnectionInfoFilePersister_PERSIST_FAILED",
                    new Object[]{persistentFile.getParent(),
                    persistentFile.getName(),
                    stackTr});
            
            String errMsg = mMessages.getString(
                    "MQConnectionInfoFilePersister_PERSIST_FAILED",
                    new Object[]{persistentFile.getParent(),
                    persistentFile.getName(),
                    stackTr});
            throw new ConnectionInfoPersistException(errMsg, t);
        }
    }
    
    synchronized public MQConnectionInfoRecord[] retrieve()
            throws ConnectionInfoPersistException {
        if (!initialized) {
            mLogger.log(Level.SEVERE,
                    "MQConnectionInfoFilePersister_NOT_INITIALIZED");
            String errMsg = mMessages.getString(
                    "MQConnectionInfoFilePersister_NOT_INITIALIZED");
            throw new ConnectionInfoPersistException(errMsg);
        }
        return (MQConnectionInfoRecord[])cache.values().toArray(new MQConnectionInfoRecord[cache.size()]);
    }
    
    synchronized public void remove(ConnectionInfoRecord[] connInfoRecords)
            throws ConnectionInfoPersistException {
        if (!initialized) {
            mLogger.log(Level.SEVERE,
                    "MQConnectionInfoFilePersister_NOT_INITIALIZED");
            String errMsg = mMessages.getString(
                    "MQConnectionInfoFilePersister_NOT_INITIALIZED");
            throw new ConnectionInfoPersistException(errMsg);
        }
        try {
            if (cache.size() > 0 &&
                    connInfoRecords != null &&
                    connInfoRecords.length > 0) {
                boolean recsUpdated = false;
                for (int i=0; i < connInfoRecords.length; i++) {
                    if (connInfoRecords[i] instanceof MQConnectionInfoRecord) {
                        cache.remove(connInfoRecords[i].hashCode());
                        connectionProps.remove(Integer.toString(connInfoRecords[i].hashCode()));
                        recsUpdated = true;
                    }
                }
                
                if (recsUpdated) {
                    forceDiskWriteRecords();
                }
            }
        } catch (Throwable t) {
            String stackTr = LogSupport.getStackTraceAsString(t);
            mLogger.log(Level.SEVERE,
                    "MQConnectionInfoFilePersister_REMOVE_FAILED",
                    new Object[]{persistentFile.getParent(),
                    persistentFile.getName(),
                    stackTr});
            
            String errMsg = mMessages.getString(
                    "MQConnectionInfoFilePersister_REMOVE_FAILED",
                    new Object[]{persistentFile.getParent(),
                    persistentFile.getName(),
                    stackTr});
            throw new ConnectionInfoPersistException(errMsg, t);
        }
    }
    
    synchronized public void close() throws ConnectionInfoPersistException {
        if (initialized) {
            try {
                forceDiskWriteRecords();
            } catch (Throwable t) {
                mLogger.log(Level.WARNING,
                        "MQConnectionInfoFilePersister_CLOSE_FAILED",
                        new Object[]{persistentFile.getParent(),
                        persistentFile.getName()});
            } finally {
                initialized = false;
            }
        }
    }
    
    private String serializeValues(MQConnectionInfoRecord mqConn) {
        return mqConn.getHostName()+
                FILE_PERSISTER_VALUES_DELIM+
                mqConn.getPortNumber()+
                FILE_PERSISTER_VALUES_DELIM+
                mqConn.getQueueManagerName()+
                FILE_PERSISTER_VALUES_DELIM+
                mqConn.getChannelName()+
                FILE_PERSISTER_VALUES_DELIM+
                mqConn.getUserName()+
                FILE_PERSISTER_VALUES_DELIM +
                mqConn.getPassword()+
                FILE_PERSISTER_VALUES_DELIM +
                mqConn.getCipherSuite()+
                FILE_PERSISTER_VALUES_DELIM +
                mqConn.getSslPeerName();
    }
    
    private void deserializeValues(String values, MQConnectionInfoRecord mqConn) {
        StringTokenizer strTok = new StringTokenizer(values, FILE_PERSISTER_VALUES_DELIM);
        int tokCount = 1;
        while (strTok.hasMoreTokens()) {
            String tok = strTok.nextToken();
            switch (tokCount) {
            case 1:
                if (tok != null) {
                    mqConn.setHostName(tok);
                }
                break;
            case 2:
                if (tok != null) {
                    try {
                        mqConn.setPortNumber(Integer.valueOf(tok));
                    } catch (NumberFormatException e) {
                        ; // use default port value
                    }
                }
                break;
            case 3:
                if (tok != null) {
                    mqConn.setQueueManagerName(tok);
                }
                break;
            case 4:
                if (tok != null) {
                    mqConn.setChannelName(tok);
                }
                break;
            case 5:
                if (tok != null) {
                    mqConn.setUserName(tok);
                }
                break;
            case 6:
                if (tok != null) {
                    mqConn.setPassword(tok);
                }
                break;
            case 7:
                if (tok != null) {
                    mqConn.setCipherSuite(tok);
                }
                break;
            case 8:
                if (tok != null) {
                    mqConn.setSslPeerName(tok);
                }
                break;
            default:
                mLogger.log(Level.WARNING,
                        "MQConnectionInfoFilePersister_UNEXPECTED_VALUE",
                        new Object[]{tok,
                        persistentFile.getParent(),
                        persistentFile.getName()});
            }
            tokCount++;
        }
    }
    
    private void forceDiskWriteRecords() throws Throwable {
        FileOutputStream fos = new FileOutputStream(persistentFile);
        connectionProps.store(fos, null);
        fos.flush();
        fos.close();
    }
    
}
