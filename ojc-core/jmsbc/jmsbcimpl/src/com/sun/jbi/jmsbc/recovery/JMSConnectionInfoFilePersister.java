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
 * @(#)JMSConnectionInfoFilePersister.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.recovery;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import java.util.Properties;
import java.util.HashMap;
import java.util.Map;
import java.util.Iterator;
import java.util.Collections;
import java.util.StringTokenizer;
import java.util.logging.Logger;
import java.util.logging.Level;

import com.sun.jbi.jmsbc.LogSupport;
import com.sun.jbi.jmsbc.util.AlertsUtil;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.internationalization.Messages;

/**
 *
 * Persists JMS connection info records. 
 * This is a simple properties file based implementation; no file locking.
 */
public class JMSConnectionInfoFilePersister implements ConnectionInfoPersister {
    private Properties connectionProps = new Properties();
    private Map cache = Collections.synchronizedMap(new HashMap());
    private File persistentFile = null;
    private boolean initialized = false;
    public static final String FILE_PERSISTER_PROPS_DIR = "file.persister.dir";
    public static final String FILE_PERSISTER_PROPS_FILENAME = "file.persister.filename";    
    public static final String FILE_PERSISTER_DEFAULT_FILENAME="com.sun.jbi.jmsbc.connections";
    
    private static final Messages mMessages =
        Messages.getMessages(JMSConnectionInfoFilePersister.class);
    private static final Logger mLogger =
        Messages.getLogger(JMSConnectionInfoFilePersister.class);

    private static final String FILE_PERSISTER_VALUES_DELIM = "|||";
    
    private static JMSConnectionInfoFilePersister singleton = null;
    
    /** Creates a new instance of JMSConnectionInfoFilePersister */
    public JMSConnectionInfoFilePersister() {
    }
    
    synchronized public void initialize(Properties persisterProps) 
    throws ConnectionInfoPersistException {
        if (initialized) {
            String errMsg = mMessages.getString(
                        "JMSBC-E0503.ConnectionInfoFileAlreadyInitialized",
                        new Object[]{persistentFile.getParent(), 
                                     persistentFile.getName()});            
            throw new ConnectionInfoPersistException (errMsg);
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
            persistentFile = new File (dir, fileStr);
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
                JMSConnectionInfoRecord jmsConn = deserializeValues(connValues);
                cache.put(Integer.valueOf(jmsConn.hashCode()), jmsConn);
            }
            initialized = true;
        } catch (Throwable t) {
            initialized = false;
            String errMsg = mMessages.getString(
                        "JMSBC-E0501.ConnectionInfoFileInitializeFailed",
                        new Object[]{dirStr, fileStr});            
            throw new ConnectionInfoPersistException(errMsg, t);            
        }        
    }

    synchronized public void persist(ConnectionInfoRecord [] connInfoRecords) 
    throws ConnectionInfoPersistException {
        if (!initialized) {
            String errMsg = mMessages.getString(
                        "JMSBC-E0502.ConnectionInfoFileNotInitialized",
                        new String [] {"persist([Lcom.sun.jbi.jmsbc.recovery.ConnectionInfoRecord;)V"});
            throw new ConnectionInfoPersistException(errMsg);
        }
        try {
            boolean recsUpdated = false;
            if (connInfoRecords != null && connInfoRecords.length > 0) {
                for (int i=0; i < connInfoRecords.length; i++) {
                    if (connInfoRecords[i] instanceof JMSConnectionInfoRecord &&
                        !cache.containsKey(Integer.valueOf(connInfoRecords[i].hashCode()))) {
                        cache.put(Integer.valueOf(connInfoRecords[i].hashCode()),
                                  connInfoRecords[i]);
                        connectionProps.put(Integer.toString(connInfoRecords[i].hashCode()),
                                            serializeValues((JMSConnectionInfoRecord)
                                                             connInfoRecords[i]));
                        recsUpdated = true;
                    }
                }
                
                if (recsUpdated) {
                    forceDiskWriteRecords();
                }
            }
        } catch (Throwable t) {
            String errMsg = mMessages.getString(
                        "JMSBC-E0504.ConnectionInfoFilePersistFailed",
                        new Object[]{persistentFile.getParent(), 
                                     persistentFile.getName()});            
            throw new ConnectionInfoPersistException(errMsg, t);                        
        }
    }

    synchronized public ConnectionInfoRecord[] retrieve() 
    throws ConnectionInfoPersistException {
        if (!initialized) {
            String errMsg = mMessages.getString(
                        "JMSBC-E0502.ConnectionInfoFileNotInitialized",
                        new String [] {"retrieve()[Lcom.sun.jbi.jmsbc.recovery.ConnectionInfoRecord;"});
            throw new ConnectionInfoPersistException(errMsg);
        }
        return (JMSConnectionInfoRecord[])cache.values().toArray(new JMSConnectionInfoRecord[cache.size()]);
    }
 
    synchronized public void remove(ConnectionInfoRecord[] connInfoRecords) 
    throws ConnectionInfoPersistException {
        if (!initialized) {
            String errMsg = mMessages.getString(
                        "JMSBC-E0502.ConnectionInfoFileNotInitialized",
                        new String [] {"remove([Lcom.sun.jbi.jmsbc.recovery.ConnectionInfoRecord;)V"});
            throw new ConnectionInfoPersistException(errMsg);
        }
        try {
            if (cache.size() > 0 && 
                connInfoRecords != null && 
                connInfoRecords.length > 0) {
                boolean recsUpdated = false;
                for (int i=0; i < connInfoRecords.length; i++) {
                    if (connInfoRecords[i] instanceof JMSConnectionInfoRecord) {
                        cache.remove(Integer.valueOf(connInfoRecords[i].hashCode()));
                        connectionProps.remove(Integer.toString(connInfoRecords[i].hashCode()));
                        recsUpdated = true;
                    }                
                }

                if (recsUpdated) {
                    forceDiskWriteRecords();
                }
            }
        } catch (Throwable t) {
            String errMsg = mMessages.getString(
                        "JMSBC-E0505.ConnectionInfoFileRemoveFailed",
                        new Object[]{persistentFile.getParent(), 
                                     persistentFile.getName()});            
            throw new ConnectionInfoPersistException(errMsg, t);                        
        }
    }
    
    synchronized public void close() throws ConnectionInfoPersistException {
        if (initialized) {
            try {
                forceDiskWriteRecords();
            } catch (Throwable t) {
                mLogger.log(Level.WARNING,
                            mMessages.getString("JMSBC-W0502.ConnectionInfoFileClosedFailed",
                              new Object[]{persistentFile.getParent(), 
                                           persistentFile.getName(),
                                           t.getLocalizedMessage()}),
                           t);
                AlertsUtil.getAlerter().warning (mMessages.getString("JMSBC-W0502.ConnectionInfoFileClosedFailed",
                              new Object[]{persistentFile.getParent(), 
                                           persistentFile.getName(),
                                           t.getLocalizedMessage()}), 
		    		                 AlertsUtil.SUN_JMS_BINDING, 
		                         null, 
		                         AlertsUtil.getServerType(),
		                         AlertsUtil.COMPONENT_TYPE_BINDING,
		                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
		                         NotificationEvent.EVENT_TYPE_ALERT,
		                         "JMSBC-W0502");
            } finally {
                initialized = false;
            }
        }
    }
    
    private String serializeValues(JMSConnectionInfoRecord jmsConn) {
        StringBuffer strBuff = new StringBuffer();
        strBuff.append(jmsConn.getConnectionURL())
               .append(FILE_PERSISTER_VALUES_DELIM)
               .append(jmsConn.getUsername())
               .append(FILE_PERSISTER_VALUES_DELIM)
               .append(jmsConn.getPassword())
               .append(FILE_PERSISTER_VALUES_DELIM)
               .append(serializeProperties(jmsConn.getJmsjcaOptions()));
        if (jmsConn instanceof JMSJndiConnectionInfoRecord) {
            JMSJndiConnectionInfoRecord jmsJndiConn = (JMSJndiConnectionInfoRecord)jmsConn;
            strBuff.append(FILE_PERSISTER_VALUES_DELIM)
                   .append(jmsJndiConn.getConnectionFactoryName())
                   .append(FILE_PERSISTER_VALUES_DELIM)
                   .append(jmsJndiConn.getProviderURL())
                   .append(FILE_PERSISTER_VALUES_DELIM)
                   .append(jmsJndiConn.getInitialContextFactory())
                   .append(FILE_PERSISTER_VALUES_DELIM)
                   .append(jmsJndiConn.getSecurityPrincipal())
                   .append(FILE_PERSISTER_VALUES_DELIM)
                   .append(jmsJndiConn.getSecurityCredentials())
                   .append(FILE_PERSISTER_VALUES_DELIM)
                   .append(serializeProperties(jmsJndiConn.getJndiEnv()));
        }
        
        return strBuff.toString();
    }

    private JMSConnectionInfoRecord deserializeValues(String values) {
        StringTokenizer strTok = new StringTokenizer(values, FILE_PERSISTER_VALUES_DELIM);
        int tokCount = 1;
        JMSConnectionInfoRecord jmsConn = null;
        while (strTok.hasMoreTokens()) {
            String tok = strTok.nextToken();
            switch (tokCount) {
                case 1:  // connectionURL
                    if (tok.equals("jndi://")) {
                        jmsConn = new JMSJndiConnectionInfoRecord();
                    } else {
                        jmsConn = new JMSConnectionInfoRecord();
                    }
                    jmsConn.setConnectionURL(tok);
                    break;
                case 2:  // username
                    if (tok != null && !tok.equalsIgnoreCase("null")) {
                        jmsConn.setUsername(tok);
                    }
                    break;
                case 3:  // password
                    if (tok != null && !tok.equalsIgnoreCase("null")) {
                        jmsConn.setPassword(tok);
                    }
                    break;
                case 4:  // jmsjcaOptions
                    if (tok != null && !tok.equalsIgnoreCase("null")) {
                        jmsConn.setJmsjcaOptions(deserializeProperties (tok));
                    }
                    break;
                case 5:  // jndi - connectionFactoryName 
                    if (tok != null && !tok.equalsIgnoreCase("null")) {
                        ((JMSJndiConnectionInfoRecord)jmsConn).setConnectionFactoryName(tok);
                    }
                    break;
                case 6:  // jndi - providerURL 
                    if (tok != null && !tok.equalsIgnoreCase("null")) {
                        ((JMSJndiConnectionInfoRecord)jmsConn).setProviderURL(tok);
                    }
                    break;
                case 7:  // jndi - initialContextFactory 
                    if (tok != null && !tok.equalsIgnoreCase("null")) {
                        ((JMSJndiConnectionInfoRecord)jmsConn).setInitialContextFactory(tok);
                    }
                    break;
                case 8:  // jndi - securityPrincipal 
                    if (tok != null && !tok.equalsIgnoreCase("null")) {
                        ((JMSJndiConnectionInfoRecord)jmsConn).setSecurityPrincipal(tok);
                    }
                    break;
                case 9:  // jndi - securityCredentials 
                    if (tok != null && !tok.equalsIgnoreCase("null")) {
                        ((JMSJndiConnectionInfoRecord)jmsConn).setSecurityCredentials(tok);
                    }                
                    break;
                case 10:  // jndi - environment properties
                    if (tok != null && !tok.equalsIgnoreCase("null")) {
                        Properties jndiEnv = deserializeProperties (tok);
                        ((JMSJndiConnectionInfoRecord)jmsConn).setJndiEnv(jndiEnv);
                    }                
                default:
                    mLogger.log(Level.WARNING,
                                "JMSBC-W0501.ConnectionInfoFileUnexpectedValueFound",
                                new Object[]{tok,
                                             persistentFile.getParent(), 
                                             persistentFile.getName()});
                    AlertsUtil.getAlerter().warning (mMessages.getString("JMSBC-W0501.ConnectionInfoFileUnexpectedValueFound",
                                new Object[]{tok,
                                             persistentFile.getParent(), 
                                             persistentFile.getName()}), 
		    		                 AlertsUtil.SUN_JMS_BINDING, 
		                         null, 
		                         AlertsUtil.getServerType(),
		                         AlertsUtil.COMPONENT_TYPE_BINDING,
		                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
		                         NotificationEvent.EVENT_TYPE_ALERT,
		                         "JMSBC-W0501");
            }
            tokCount++;
        }
        return jmsConn;
    }

    private String serializeProperties (Properties props) {
    	if(props == null || props.size() == 0)
    		return "null";
        // JNDI env entries are stored in format "n1=v1;n2=v2;...;nn=vn;"
        StringBuffer strBuff = new StringBuffer();
        if (props != null && props.size() > 0) {
            Iterator keyIter = props.keySet().iterator();
            while (keyIter.hasNext()) {
                String envName = (String)keyIter.next();
                String envVal = (String)props.getProperty(envName);
                strBuff.append(envName)
                       .append("=")
                       .append(envVal)
                       .append(";");
            }
        }
        
        return strBuff.toString();
    }

    private Properties deserializeProperties (String tok) {
        Properties jndiEnv = new Properties();
        StringTokenizer strTok = new StringTokenizer(tok, ";");
        while (strTok.hasMoreTokens()) {
            String aTok = strTok.nextToken();
            StringTokenizer envTok = new StringTokenizer(aTok, "=");
            String envName = envTok.nextToken();
            String envVal = envTok.nextToken();
            jndiEnv.setProperty(envName, envVal);
        }
        return jndiEnv;
    }
    
    private void forceDiskWriteRecords () throws Throwable {
        FileOutputStream fos = new FileOutputStream(persistentFile);
        connectionProps.store(fos, null);
        fos.flush();
        fos.close();                        
    }

}
