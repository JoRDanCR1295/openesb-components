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
 * @(#)BPELSEInstallerConfigurationMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel;


public interface BPELSEInstallerConfigurationMBean {

    public Boolean getDebugEnabled();

    public void setDebugEnabled(Boolean debugFlag);

    public Integer getDebugPort();

    public void setDebugPort(Integer debugPort);

    public Boolean getPersistenceEnabled();

    public void setPersistenceEnabled(Boolean persistenceEnabled);

    public String getDatabaseNonXAJNDIName();

    public void setDatabaseNonXAJNDIName(String jndiname);

    public String getDatabaseXAJNDIName();

    public void setDatabaseXAJNDIName(String jndiname);

    public Integer getThreadCount();

    public void setThreadCount(Integer maxThreadCount);

    public Integer getEngineExpiryInterval();

    public void setEngineExpiryInterval(Integer interval);
    
    public Integer getWaitingRequestLifeSpan();
    
    public void setWaitingRequestLifeSpan(Integer waitingRequestLifeSpan);

    public Boolean getMonitoringEnabled();

    public void setMonitoringEnabled(Boolean flag);

    public Boolean getMonitoringVariableEnabled();

    public void setMonitoringVariableEnabled(Boolean flag);

    public Boolean getKPIEnabled();

    public void setKPIEnabled(Boolean flag);

    public String getTransformEngine();

    public void setTransformEngine(String engine);

    /**
     * sets validation enabled flag value
     * @param validationEnabled - validation enabled flag
     */
    public void setValidationEnabled(Boolean validationEnabled);

    /**
     * returns validation enabled flag
     * @return ValidationEnabled - validation enabled flag
     */
    public Boolean getValidationEnabled();
}