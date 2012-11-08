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
 * @(#)BPELSEInstallerConfiguration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel;

import javax.management.NotCompliantMBeanException;
import javax.management.StandardMBean;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine.TransformEngine;


/**
 * Implements the InstallerConfigurationMBean.  
 * Handles the initialization, default values, and runtime settings for the JBI Engine
 *
 * @author Sun Microsystems
 */
public class BPELSEInstallerConfiguration extends StandardMBean implements BPELSEInstallerConfigurationMBean {

    private ComponentConfig props;
    
    private Boolean mDebugEnabled;
    private Integer mDebugPort;

    private Boolean mPersistenceEnabled;
    private String mXAJNDIName;
    private String mNonXAJNDIName;
    
    private Integer mThreadCount;
    private Integer mEngineExpiryInterval;
    private Integer mWaitingRequestLifeSpan;
    
    private Boolean mMonitoringEnabled;
    private Boolean mMonitoringVariableEnabled;
    private Boolean mKPIEnabled;

    private Boolean mValidationEnabled;
    
    private String mTransformEngine;
    
    /**
     * default constructor
     *
     * @throws NotCompliantMBeanException not compliant MBean exception
     */
    public BPELSEInstallerConfiguration() throws NotCompliantMBeanException {
        super(BPELSEInstallerConfigurationMBean.class);
    }

    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#getDebugEnabled()
     */
    public Boolean getDebugEnabled() {
        return mDebugEnabled;
    }

    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#setDebugEnabled(java.lang.Boolean)
     */
    public void setDebugEnabled(Boolean debugFlag) {
        if (debugFlag == null) {
            debugFlag = Engine.DEBUG_ENABLED_FACCTORYDEFAULT;
        }
        mDebugEnabled = debugFlag;
    }

    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#getDebugPort()
     */
    public Integer getDebugPort() {
        return mDebugPort;
    }

    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#setDebugPort(java.lang.Integer)
     */
    public void setDebugPort(Integer debugPort) {
        // invalid number, set to default
        if ((debugPort == null) || (debugPort < 1)) {
            mDebugPort = Engine.DEBUG_PORT_FACCTORYDEFAULT;
        }
        mDebugPort = debugPort;
    }

    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#getPersistenceEnabled()
     */
    public Boolean getPersistenceEnabled() {
        return mPersistenceEnabled;
    }

    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#setPersistenceEnabled(java.lang.Boolean)
     */
    public void setPersistenceEnabled(Boolean persistenceEnabled) {
		if (persistenceEnabled == null) {
		    persistenceEnabled = Engine.PERSISTENCE_ENABLED_FACCTORYDEFAULT;
        }
        mPersistenceEnabled = persistenceEnabled;
    }

    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#getDatabaseNonXAJNDIName()
     */
    public String getDatabaseNonXAJNDIName() {
        return mNonXAJNDIName;
    }

    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#setDatabaseNonXAJNDIName(java.lang.String)
     */
    public void setDatabaseNonXAJNDIName(String jndiname) {
        if (jndiname == null) {
            jndiname = Engine.DB_NONXA_JNDI_NAME_FACCTORYDEFAULT;
        }
        mNonXAJNDIName = jndiname;
    }

    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#getDatabaseXAJNDIName()
     */
    public String getDatabaseXAJNDIName() {
        return mXAJNDIName;
    }

    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#setDatabaseXAJNDIName(java.lang.String)
     */
    public void setDatabaseXAJNDIName(String jndiname) {
    	if (jndiname == null) {
            jndiname = Engine.DB_XA_JNDI_NAME_FACCTORYDEFAULT;
        }
        mXAJNDIName = jndiname;
    }
    

    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#getThreadCount()
     */
    public Integer getThreadCount() {
        return mThreadCount;
    }

    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#setThreadCount(java.lang.Integer)
     */
    public void setThreadCount(Integer maxThreadCount) {
        if((maxThreadCount == null) || (maxThreadCount < 1)){
            mThreadCount = Engine.THREAD_COUNT_FACCTORYDEFAULT;
        }
        mThreadCount = maxThreadCount;
    }

    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#getEngineExpiryInterval()
     */
    public Integer getEngineExpiryInterval() {
        return mEngineExpiryInterval;
    }

    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#setEngineExpiryInterval(java.lang.Integer)
     */
    public void setEngineExpiryInterval(Integer interval) {
    	if (interval == null) {
            interval = Engine.ENGINE_EXPIRY_INTERVAL_FACCTORYDEFAULT;
        }
        mEngineExpiryInterval = interval;
    }
    
    /*
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#getWaitingRequestLifeSpan()
     */
    public Integer getWaitingRequestLifeSpan() {
    	return mWaitingRequestLifeSpan;
    }
    
    /*
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#setWaitingRequestLifeSpan(java.lang.Integer)
     */
    public void setWaitingRequestLifeSpan(Integer waitingRequestLifeSpan) {
    	if (waitingRequestLifeSpan == null) {
    		waitingRequestLifeSpan = Engine.WAITING_REQUEST_LIFE_SPAN_FACTORYDEFAULT;
    	}
    	mWaitingRequestLifeSpan = waitingRequestLifeSpan;
    }

	/*
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#getMonitorEnabled()
     */
    public Boolean getMonitoringEnabled() {
        return mMonitoringEnabled;
    }
	
    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#setMonitorEnabled(java.lang.Boolean)
     */
    public void setMonitoringEnabled(Boolean flag) {
        if (flag == null) {
            flag = Engine.MONITOR_ENABLED_FACCTORYDEFAULT;
        }
        mMonitoringEnabled = flag;     
    }

    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#getMonitoringVariableEnabled()
     */
    public Boolean getMonitoringVariableEnabled() {
        return mMonitoringVariableEnabled;
    }

    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#setMonitoringVariableEnabled(java.lang.Boolean)
     */
    public void setMonitoringVariableEnabled(Boolean flag) {
        if (flag == null) {
            flag = Engine.MONITOR_ENABLED_FACCTORYDEFAULT;
        }

        mMonitoringVariableEnabled = flag;
    }
    
    /* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#getKPIEnabled()
     */
    public Boolean getKPIEnabled() {
        return mKPIEnabled;
	}	
	
	/** @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#getTransformEngine() */
	public String getTransformEngine() {
		return (mTransformEngine == null) ? TransformEngine.XSLT_1_0.toString() : mTransformEngine;
	}

	/** @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#setTransformEngine(java.lang.String) */
	public void setTransformEngine(String engine) {
		if (engine == null) {
			mTransformEngine = TransformEngine.XSLT_1_0.toString();
			return;
		}
		
		try {
			TransformEngine te = TransformEngine.valueOf(engine);
			mTransformEngine = (te == null) ? TransformEngine.XSLT_1_0.toString()
											: te.toString();
		}
		catch (Exception e) {
			mTransformEngine = TransformEngine.XSLT_1_0.toString();
		}
	}

	/* 
     * @see com.sun.jbi.engine.bpel.BPELSEInstallerConfigurationMBean#setKPIEnabled(java.lang.Boolean)
     */
	public void setKPIEnabled(Boolean flag) {
		if (flag == null) {
        	flag = Engine.KPI_ENABLED_FACCTORYDEFAULT;
        }
        mKPIEnabled = flag;		
	}

	public void setValues(ComponentConfig props) {
        this.props = props;
        
        mDebugEnabled = Boolean.valueOf(props.getProperty(Engine.DEBUG_ENABLED).getValue());
        mDebugPort = Integer.valueOf(props.getProperty(Engine.DEBUG_PORT).getValue());
        
        mPersistenceEnabled = Boolean.valueOf(props.getProperty(Engine.PERSISTENCE_ENABLED).getValue());
        
        mNonXAJNDIName = props.getProperty(Engine.DB_NON_XA_JNDI_NAME).getValue();
        mXAJNDIName = props.getProperty(Engine.DB_XA_JNDI_NAME).getValue();

        mThreadCount = Integer.valueOf(props.getProperty(Engine.THREAD_COUNT).getValue());
        mEngineExpiryInterval = Integer.valueOf(props.getProperty(Engine.ENGINE_EXPIRY_INTERVAL).getValue());
        mWaitingRequestLifeSpan = Integer.valueOf(props.getProperty(Engine.WAITING_REQUEST_LIFE_SPAN).getValue());
        
        mMonitoringEnabled = Boolean.valueOf(props.getProperty(Engine.MONITOR_ENABLED).getValue());
        mMonitoringVariableEnabled = Boolean.valueOf(props.getProperty(Engine.MONITOR_VARIABLE_ENABLED).getValue());
        mKPIEnabled = Boolean.valueOf(props.getProperty(Engine.KPI_ENABLED).getValue());
        setTransformEngine(props.getProperty(Engine.TRANSFORM_ENGINE).getValue());
        mValidationEnabled = Boolean.valueOf(props.getProperty(Engine.VALIDATION_ENABLED).getValue());
    }

    public ComponentConfig getValues() {
        props.getProperty(Engine.DEBUG_ENABLED).setValue(mDebugEnabled.toString());
        props.getProperty(Engine.DEBUG_PORT).setValue(mDebugPort.toString());

        props.getProperty(Engine.PERSISTENCE_ENABLED).setValue(mPersistenceEnabled.toString());
        props.getProperty(Engine.DB_NON_XA_JNDI_NAME).setValue(mNonXAJNDIName);
        props.getProperty(Engine.DB_XA_JNDI_NAME).setValue(mXAJNDIName);
        
        props.getProperty(Engine.THREAD_COUNT).setValue(mThreadCount.toString());
        props.getProperty(Engine.ENGINE_EXPIRY_INTERVAL).setValue(mEngineExpiryInterval.toString());
        props.getProperty(Engine.WAITING_REQUEST_LIFE_SPAN).setValue(mWaitingRequestLifeSpan.toString());

        props.getProperty(Engine.MONITOR_ENABLED).setValue(mMonitoringEnabled.toString());        
        props.getProperty(Engine.KPI_ENABLED).setValue(mKPIEnabled.toString());               
        props.getProperty(Engine.MONITOR_VARIABLE_ENABLED).setValue(mMonitoringVariableEnabled.toString()); 
        
        props.getProperty(Engine.TRANSFORM_ENGINE).setValue(mTransformEngine);
        
        props.getProperty(Engine.VALIDATION_ENABLED).setValue(mValidationEnabled.toString());
        return props;
    }

    public void setValidationEnabled(Boolean validationEnabled) {
        if (validationEnabled == null) {
            validationEnabled = Engine.VALIDATION_ENABLED_FACCTORYDEFAULT;
        }
        mValidationEnabled = validationEnabled;
    }

    public Boolean getValidationEnabled() {
        return mValidationEnabled;
    }
}
