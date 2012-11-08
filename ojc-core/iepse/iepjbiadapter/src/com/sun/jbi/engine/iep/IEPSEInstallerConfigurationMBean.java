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
 * @(#)IEPSEInstallerConfigurationMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep;

/**
 * IEPSEInstallerConfigurationMBean.java
 * 
 * Created on May 23, 2005, 1:44 PM
 * 
 * 
 * 
 * @author Bing Lu
 */
public interface IEPSEInstallerConfigurationMBean {
    public static final String SUCCEED = "SUCCEED";
    public static final String FAIL = "FAIL";
    
    public void setEngineExpiryInterval(String engineExpiryInterval);
    public void setDatabaseNonXaJndiName(String value);
    public void setDatabaseXaJndiName(String value);
    public void setDatabaseSchemaName(String value);
    
//    UNCOMMENT when AI engine plugin is enabled
//    public void setAieType(String value);
//    public void setAieHostname(String value);
//    public void setAiePort(String value);
//    public void setAieId(String value);
    
    public void setRuntimeStyle(String value);
    public void setGarbageCollectionEnabled(String value);
    
    public String getEngineExpiryInterval();
    public String getDatabaseNonXaJndiName();
    public String getDatabaseXaJndiName();
    public String getDatabaseSchemaName();
    public String getAieType();
    public String getAieHostname();
    public String getAiePort();
    public String getAieId();
    public String getRuntimeStyle();
    public String getGarbageCollectionEnabled();
    
//    public void setNoOfThreads(String value);
//    public String getNoOfThreads() ;
    
    public void setTransactedOutput(String value);
    public String getTransactedOutput();
    
    public void setMaximumBatchSize(String value);
    public String getMaximumBatchSize();
    
}    
