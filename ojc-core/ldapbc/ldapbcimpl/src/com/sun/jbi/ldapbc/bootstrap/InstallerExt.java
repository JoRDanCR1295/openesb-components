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

package com.sun.jbi.ldapbc.bootstrap;

import java.util.logging.Logger;
import java.util.logging.Level;
import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.internationalization.Messages;


/**
 * Installer Extension MBean, allow configuration to be changed before installation
 */
public class InstallerExt implements InstallerExtMBean {
    private static final Messages mMessages = Messages.getMessages(InstallerExt.class);
    Integer mThreads;
	Integer mRetryCount;
	Integer mRetryInterval;
	String mRecoveryType;
	Boolean mAllowDynamicEndpoint;
    String mUrl;
    String mUsername;
    String mPassword;
    private Logger mLogger;
    private ComponentConfig mConfigProps;
    Boolean mAllowConnectionPooling;
	Integer mConnectionPoolPrefSize;
	Integer mConnectionPoolMaxSize;
	Integer mConnectionMaxIdleTimeout;
	String mConnectionProtocol;
	String mConnectionAuthentication;
    
 // Configuration validation settings    
    private static int MIN_THREADS = 1;
    private static int MAX_THREADS = 10000;
    
	private static int MIN_RETRIES = 0;
	private static int MIN_INTERVAL = 1000;

	// Connection Pool default settings
	private static int PREF_CONN_POOL_SIZE = 1;
	private static int MAX_CONN_POOL_SIZE = 10;
	private static int MAX_CONN_POOL_IDLE_TIMEOUT = 300;

    /** Creates a new instance of InstallerExt */
    public InstallerExt() {
        mLogger = Messages.getLogger(InstallerExt.class);
    }

    /**
     *
     */
    public Integer getThreads() {
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00101.LDLC_Getthreads_Called", new Object[] { mThreads }));
        return mThreads != null ? mThreads : new Integer(MIN_THREADS);
        
    }
    
	public Integer getRetryCount(){
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00109.LDLC_GetRetryCount_Called", new Object[] { mRetryCount }));
		return mRetryCount != null ? mRetryCount : new Integer(MIN_RETRIES);
	}

	public Integer getRetryInterval(){
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00110.LDLC_GetRetryInterval_Called", new Object[] { mRetryInterval }));
		return mRetryInterval != null ? mRetryInterval : new Integer(MIN_INTERVAL);
	}

	public String getRecoveryType(){
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00111.LDLC_GetRecoveryType_Called", new Object[] { mRecoveryType }));
		return mRecoveryType; 
			//!= null ? mRecoveryType : new Integer(MIN_INTERVAL);
	}
	
	/**
	 * Returns whether Dynamic Endpoint is enabled or not
	 */
	public Boolean getAllowDynamicEndpoint(){
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00117.LDLC_GetAllowDynamicEndpoint_Called", new Object[] { mAllowDynamicEndpoint.toString()}));
		return mAllowDynamicEndpoint; 
	}	
	
	
    /**
     * Ldap connection url
     */
    public String getUrl() {
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00103.LDLC_GetUrl_Called", new Object[] { mUrl } ));
        
        return mUrl;
    }

    /**
     *
     */
    public String getUsername() {
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00104.LDLC_GetUsername_Called", new Object[] { mUsername } ));
        
        return mUsername;
    }
    
    /**
     *
     */
    public String getPassword() {
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00105.LDLC_GetPassword_Called", new Object[] { mPassword } ));
        return mPassword;
    }
    
    /**
	 * This method returns whether Connection Pooling is enabled
     * @returns Boolean
     */
	public Boolean getAllowConnectionPooling(){
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00125.LDLC_GetAllowConnectionPooling_Called", new Object[] { mAllowConnectionPooling.toString() }));
		return mAllowConnectionPooling;
	}

    /**
	 * This method returns Connection Pool Minimum Size
     * @returns Integer
     */
	public Integer getConnectionPoolPrefSize(){
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00119.LDLC_GetConnPoolPrefSize_Called", new Object[] { mConnectionPoolPrefSize }));
		return mConnectionPoolPrefSize != null ? mConnectionPoolPrefSize : new Integer(PREF_CONN_POOL_SIZE);
	}

	 /**
	 * This method returns Connection Pool Maximum Size
     * @returns Integer
     */
	public Integer getConnectionPoolMaxSize(){
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00120.LDLC_GetConnPoolMaxSize_Called", new Object[] { mConnectionPoolMaxSize }));
		return mConnectionPoolMaxSize != null ? mConnectionPoolMaxSize : new Integer(MAX_CONN_POOL_SIZE);
	}

	 /**
	 * This method returns Connection Pool Maximum Idle Timeout
     * @returns Integer
     */
	public Integer getConnectionMaxIdleTimeout(){
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00123.LDLC_GetConnPoolMaxIdleTimeout_Called", new Object[] { mConnectionMaxIdleTimeout }));
		return mConnectionMaxIdleTimeout != null ? mConnectionMaxIdleTimeout : new Integer(MAX_CONN_POOL_IDLE_TIMEOUT);
	}

	 /**
	 * This method returns Connection Pool Protocol
     * @returns String
     */
	public String getConnectionProtocol(){
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00127.LDLC_GetConnProtocol_Called", new Object[] { mConnectionProtocol }));
		return mConnectionProtocol;
	}

	 /**
	 * This method returns Connection Pool Authentication
     * @returns String
     */
	public String getConnectionAuthentication(){
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00129.LDLC_GetConnAuthentication_Called", new Object[] { mConnectionAuthentication }));
		return mConnectionAuthentication;
	}

    /**
     *
     */
    public void setThreads(final Integer val) {
	   if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00102.LDLC_Setthreads_Called", new Object[] { val }));
        mThreads = val;
    }

	 
	 public void setRetryCount(final Integer val) {
	   if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00111.LDLC_SetRetryCount_Called", new Object[] { val }));
        mRetryCount = val;
    }
	 			
	public void setRetryInterval(final Integer val) {
	   if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00112.LDLC_SetRetryInterval_Called", new Object[] { val }));
        mRetryInterval = val;
    }
	
	public void setRecoveryType(String name) {
	   if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00113.LDLC_SetRecoveryType_Called", new Object[] { name }));
       mRecoveryType  = name;
    }
	
	/**
	 * Specifies whether Dynamic Endpoint is enabled or not
	 * @param val boolean
	 */
	public void setAllowDynamicEndpoint(Boolean val) {
		   if (mLogger.isLoggable(Level.INFO))
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00118.LDLC_SetAllowDynamicEndpoint_Called", new Object[] { val.toString() }));
	       mAllowDynamicEndpoint  = val;
	}
	    
    
    /**
     * set Ldap Url
     */
    public void setUrl(String url) {
		  if (mLogger.isLoggable(Level.INFO))
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00106.LDLC_SetUrl_Called ", new Object[] { url }));	
        mUrl = url;
    }
    
    /**
     *
     */
    public void setUsername(String name) {
		  if (mLogger.isLoggable(Level.INFO))
			 mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00107.LDLC_setUsername_Called",new Object[] { name }));
        mUsername = name;
    }
    
    /**
     *
     */
    public void setPassword(String pass) {
          if (mLogger.isLoggable(Level.INFO))
			 mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00108.LDLC_setPassword_Called",new Object[] { pass }));
        mPassword = pass;
    }

    /**
	 * This method sets Connection Pooling
     * @param Boolean
     */
	public void setAllowConnectionPooling(final Boolean connPool){
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00126.LDLC_SetAllowConnectionPooling_Called", new Object[] { connPool.toString() }));
		mAllowConnectionPooling = connPool;
	}

    /**
	 * This method sets Connection Pool Minimum Size
     * @param Integer
     */
	public void setConnectionPoolPrefSize(final Integer prefSize){
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00121.LDLC_SetConnPoolPrefSize_Called", new Object[] { prefSize }));
		mConnectionPoolPrefSize = prefSize;
	}

	 /**
	 * This method sets Connection Pool Maximum Size
     * @param Integer
     */
	public void setConnectionPoolMaxSize(final Integer maxSize){
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00122.LDLC_SetConnPoolMaxSize_Called", new Object[] { maxSize }));
		mConnectionPoolMaxSize = maxSize; 
	}

	 /**
	 * This method sets Connection Pool Maximum Idle Timeout
     * @param Integer
     */
	public void setConnectionMaxIdleTimeout(final Integer maxIdleTime){
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00124.LDLC_SetConnPoolMaxIdleTimeout_Called", new Object[] { maxIdleTime }));
		mConnectionMaxIdleTimeout = maxIdleTime; 
	}

	 /**
	 * This method sets Connection Pool Protocol
     * @param String
     */
	public void setConnectionProtocol(final String protocol){
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00128.LDLC_SetConnProtocol_Called", new Object[] { protocol }));
		mConnectionProtocol = protocol; 
	}

	 /**
	 * This method sets Connection Pool Authentication
     * @param String
     */
	public void setConnectionAuthentication(final String auth){
		if(mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00130.LDLC_SetConnAuthentication_Called", new Object[] { auth }));
		mConnectionAuthentication = auth; 
	}

    public void setInitialConfigurations(ComponentConfig props) {
		
        if(props.getProperty(CONFIG_THREADS).getValue()==null){
            mThreads = 10;
        } else {
            mThreads = Integer.valueOf(props.getProperty(CONFIG_THREADS).getValue());
        }

        if (props.getProperty(CONFIG_MAX_RETRIES_COUNT).getValue() ==  null) {
            mRetryCount = MIN_RETRIES;
        } else {
            mRetryCount = Integer.valueOf(props.getProperty(CONFIG_MAX_RETRIES_COUNT).getValue());
        }
            
        if (props.getProperty(CONFIG_RETRY_INTERVAL).getValue() == null) {
            mRetryInterval = MIN_INTERVAL;
        } else {
            mRetryInterval = Integer.valueOf(props.getProperty(CONFIG_RETRY_INTERVAL).getValue());
        }

		if(props.getProperty(CONFIG_RECOURCE_ACTION).getValue() == null){
		    mRecoveryType = "ERROR";
		}else{
			mRecoveryType = props.getProperty(CONFIG_RECOURCE_ACTION).getValue();
		}
		
		if(props.getProperty(CONFIG_ALLOW_DYNAMIC_ENDPOINT).getValue() == null){
			mAllowDynamicEndpoint = Boolean.FALSE;
		}else{
			mAllowDynamicEndpoint = Boolean.valueOf(props.getProperty(CONFIG_ALLOW_DYNAMIC_ENDPOINT).getValue());
		}
		
		if (props.getProperty(CONFIG_ALLOW_CONNECTION_POOLING).getValue() == null) {
			mAllowConnectionPooling = Boolean.FALSE;
        } else {
        	mAllowConnectionPooling = Boolean.valueOf(props.getProperty(CONFIG_ALLOW_CONNECTION_POOLING).getValue());
        }

		if (props.getProperty(CONFIG_CONNPOOL_PREF_SIZE).getValue() == null) {
            mConnectionPoolPrefSize = PREF_CONN_POOL_SIZE;
        } else {
            mConnectionPoolPrefSize = Integer.valueOf(props.getProperty(CONFIG_CONNPOOL_PREF_SIZE).getValue());
        }

		if (props.getProperty(CONFIG_CONNPOOL_MAX_SIZE).getValue() == null) {
            mConnectionPoolMaxSize = MAX_CONN_POOL_SIZE;
        } else {
            mConnectionPoolMaxSize = Integer.valueOf(props.getProperty(CONFIG_CONNPOOL_MAX_SIZE).getValue());
        }

		if (props.getProperty(CONFIG_CONNPOOL_MAX_IDLE_TIME).getValue() == null) {
            mConnectionMaxIdleTimeout = MAX_CONN_POOL_IDLE_TIMEOUT;
        } else {
            mConnectionMaxIdleTimeout = Integer.valueOf(props.getProperty(CONFIG_CONNPOOL_MAX_IDLE_TIME).getValue());
        }

		if (props.getProperty(CONFIG_CONNPOOL_PROTOCOL).getValue() == null) {
            mConnectionProtocol = "plain ssl";
        } else {
            mConnectionProtocol = props.getProperty(CONFIG_CONNPOOL_PROTOCOL).getValue();
        }

		if (props.getProperty(CONFIG_CONNPOOL_AUTHENTICATION).getValue() == null) {
            mConnectionAuthentication = "none simple";
        } else {
            mConnectionAuthentication = props.getProperty(CONFIG_CONNPOOL_AUTHENTICATION).getValue();
        }

        mConfigProps = props;
    }

    public ComponentConfig getInstallationConfigurationProperties() {
        mConfigProps.getProperty(CONFIG_THREADS).setValue(getThreads().toString());
        mConfigProps.getProperty(CONFIG_MAX_RETRIES_COUNT).setValue(getRetryCount().toString());
        mConfigProps.getProperty(CONFIG_RETRY_INTERVAL).setValue(getRetryInterval().toString());
		mConfigProps.getProperty(CONFIG_RECOURCE_ACTION).setValue(getRecoveryType());
		mConfigProps.getProperty(CONFIG_ALLOW_DYNAMIC_ENDPOINT).setValue(getAllowDynamicEndpoint().toString());
		mConfigProps.getProperty(CONFIG_ALLOW_CONNECTION_POOLING).setValue(getAllowConnectionPooling().toString());
		mConfigProps.getProperty(CONFIG_CONNPOOL_PREF_SIZE).setValue(getConnectionPoolPrefSize().toString());
		mConfigProps.getProperty(CONFIG_CONNPOOL_MAX_SIZE).setValue(getConnectionPoolMaxSize().toString());
		mConfigProps.getProperty(CONFIG_CONNPOOL_MAX_IDLE_TIME).setValue(getConnectionMaxIdleTimeout().toString());
		mConfigProps.getProperty(CONFIG_CONNPOOL_PROTOCOL).setValue(getConnectionProtocol());
		mConfigProps.getProperty(CONFIG_CONNPOOL_AUTHENTICATION).setValue(getConnectionAuthentication());
        return mConfigProps;
    }
}