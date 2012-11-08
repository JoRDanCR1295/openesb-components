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
 * @(#)IEPSERuntimeConfigurationMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep;

/*
 * BPELSEConfigurationMBean.java
 *
 * Created on May 19, 2005, 7:05 PM
 *
 * To change this template, choose Tools | Options and locate the template under
 * the Source Creation and Management node. Right-click the template and choose
 * Open. You can then make changes to the template in the Source Editor.
 */

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public interface IEPSERuntimeConfigurationMBean {
    public void setEngineExpiryInterval(Integer engineExpiryInterval) throws InvalidAttributeValueException, MBeanException;
    
    public void setDatabaseNonXaJndiName(String dbJndiName) throws InvalidAttributeValueException, MBeanException;
    public void setDatabaseXaJndiName(String dbJndiName) throws InvalidAttributeValueException, MBeanException;
    public void setDatabaseSchemaName(String dbSchema) throws InvalidAttributeValueException, MBeanException;
    public void setGarbageCollectionEnabled(Boolean value) throws InvalidAttributeValueException, MBeanException;

    public Integer getEngineExpiryInterval();
    public String getDatabaseNonXaJndiName();
    public String getDatabaseXaJndiName();
    public String getDatabaseSchemaName();
    public Boolean getGarbageCollectionEnabled();
    
//    public void setNoOfThreads(Integer value) throws InvalidAttributeValueException, MBeanException;
//    public Integer getNoOfThreads() ;
   
    public void setTransactedOutput(Boolean value) throws InvalidAttributeValueException, MBeanException;
    public Boolean getTransactedOutput() ;
    
    public void setMaximumBatchSize(Integer value) throws InvalidAttributeValueException, MBeanException;
    public Integer getMaximumBatchSize();
    
    /**
     * Retrieves the configuration display schema
     */
    public String retrieveConfigurationDisplaySchema();
    
    /**
     * Retrieves the configuration display data
     */
    public String retrieveConfigurationDisplayData();
    
}
