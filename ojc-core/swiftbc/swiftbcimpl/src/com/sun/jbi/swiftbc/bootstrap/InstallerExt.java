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
 * @(#)InstallerExt.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.bootstrap;

import java.util.logging.Logger;
import java.util.logging.Level;
import com.sun.jbi.internationalization.Messages;

/**
 * Installer Extension MBean, allow configuration to be changed before installation
 * 
 * @author aegloff
 */
public class InstallerExt implements InstallerExtMBean {

    private static final Messages messages = Messages.getMessages(InstallerExt.class);

    private Logger mLogger;

    /** Database URL setting when there is no setting defined in jbi.xml */
    private static final String DB_URL_FACTORYDEFAULT = "jdbc:derby://localhost:1527/sample"; //$NON-NLS-1$

    /** Database type setting when there is no setting defined in jbi.xml */
    private static final String DB_TYPE_FACTORYDEFAULT = "4"; //$NON-NLS-1$

    /** Database JNDI Name setting when there is no setting defined in jbi.xml */
    private static final String DB_JNDI_NAME_FACTORYDEFAULT = "jdbc/__defaultDS"; //$NON-NLS-1$

    /** Database user name setting when there is no setting defined in jbi.xml */
    public static final String DB_USERNAME_FACTORYDEFAULT = "app"; //$NON-NLS-1$

    /** Database password setting when there is no setting defined in jbi.xml */
    public static final String DB_PASSWORD_FACTORYDEFAULT = "app"; //$NON-NLS-1$

    /** Database JNDI Name property tag string pattern */
    String DB_JNDI_NAME = "DB_JNDIName"; //$NON-NLS-1$

    /** Username property tag string pattern */
    String DB_USERNAME = "DB_UserName"; //$NON-NLS-1$

    /** password property tag string pattern */
    String DB_PASSWORD = "DB_Password"; //$NON-NLS-1$

    /** databse URL property tag string pattern */
    String DB_URL = "DB_URL"; //$NON-NLS-1$

    /** database type property tag string pattern */
    String DB_TYPE = "DB_Type"; //$NON-NLS-1$

    private String mThreads;

    private String mDBURL;

    private String mDBType;

    private String mUser;

    private String mPassWD;

    private String mJNDIName;

    /** Creates a new instance of InstallerExt */
    public InstallerExt() {
        mLogger = Messages.getLogger(InstallerExt.class);
    }

    public String getThreads() {
        mLogger.info("Get_threads" + mThreads);
        return mThreads;
    }

    /**
     * setter for number of Threads
     *
     * @param val number of threads
     */
    public void setThreads(String val) {
        mLogger.info("Set_threads" + val);
        mThreads = val;
    }
	 /**
     * setter for database url.
     * Does not allow for null value settings.  Instead returns FACTORYDEFAULT value.
     *
     * @param url database url
     */
    public void setDB_URL(String url) {
        mLogger.log(Level.INFO, 
                messages.getString("Setting_dburl") + url);
        if (url == null) {
            url = DB_URL_FACTORYDEFAULT;
        }
        mDBURL = url;
    }
	  /**
     * getter for database url
     * Does not allow for null value settings.  Instead returns FACTORYDEFAULT value.
     *
     * @return String database url
     */
    public String getDB_URL() {
        if (mDBURL == null) {
            mDBURL = DB_URL_FACTORYDEFAULT;
        }
        return mDBURL;
    }

	/**
     * getter for database type
     * Does not allow for null value settings.  Instead returns FACTORYDEFAULT value.
     *
     * @return String database type
     */
    public String getDB_Type() {
        if (mDBType == null) {
            mDBType = DB_TYPE_FACTORYDEFAULT;
        }
        return mDBType;
    }

    /**
     * setter for database type
     * Does not allow for null value settings.  Instead returns FACTORYDEFAULT value.
     *
     * @param type database type
     */
    public void setDB_Type(String type) {
        mLogger.log(Level.INFO, messages.getString("Setting_type") + type);
        if (type == null) {
            type = DB_TYPE_FACTORYDEFAULT;
        }
        mDBType = type;
    }
	 /**
     * setter for database user
     * Does not allow for null value settings.  Instead returns FACTORYDEFAULT value.
     *
     * @param user database user name
     */
    public void setDB_UserName(String user) {
        mLogger.log(Level.INFO, messages.getString("Setting_user") + user);
        if (user == null) {
            user = DB_USERNAME_FACTORYDEFAULT;
        }
        mUser = user;
    }

    /**
     * setter for database password
     * Does not allow for null value settings.  Instead returns FACTORYDEFAULT value.
     *
     * @param passwd database password
     */
    public void setDB_Password(String passwd) {
        mLogger.log(Level.INFO, messages.getString("Setting_password") + passwd);
        if (passwd == null) {
            passwd = DB_PASSWORD_FACTORYDEFAULT;
        }
        mPassWD = passwd;
    }

    /**
     * setter for JNDI name
     * Does not allow for null value settings.  Instead returns FACTORYDEFAULT value.
     *
     * @param jndiname jndi name
     */
    public void setDB_JNDIName(String jndiname) {
        mLogger.log(Level.INFO, messages.getString("Setting_jndiname") + jndiname);
        if (jndiname == null) {
            jndiname = DB_JNDI_NAME_FACTORYDEFAULT;
        }
        mJNDIName = jndiname;
    }

    /**
     * getter for DB user name
     * Does not allow for null value settings.  Instead returns FACTORYDEFAULT value.
     *
     * @return String database user name
     */
    public String getDB_UserName() {
        if (mUser == null) {
            mUser = DB_USERNAME_FACTORYDEFAULT;
        }
        return mUser;
    }

    /**
     * getter for DB password
     * Does not allow for null value settings.  Instead returns FACTORYDEFAULT value.
     *
     * @return String database password
     */
    public String getDB_Password() {
        if (mPassWD == null) {
            mPassWD = DB_PASSWORD_FACTORYDEFAULT;
        }
        return mPassWD;
    }
	 /**
     * getter for JNDI name
     * Does not allow for null value settings.  Instead returns FACTORYDEFAULT value.
     *
     * @return mJNDIName JNDI name
     */
    public String getDB_JNDIName() {
        if (mJNDIName == null) {
            mJNDIName = DB_JNDI_NAME_FACTORYDEFAULT;
        }
        return mJNDIName;
    }
}
