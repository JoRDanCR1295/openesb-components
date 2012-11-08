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
 * @(#)RuntimeConfigurationMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.TabularData;
import java.util.Map;

/**
 * MBean interface for run-time configuration
 * @author aegloff
 */
public interface RuntimeConfigurationMBean {
    public Integer getThreads();
    public void setThreads(Integer val) throws InvalidAttributeValueException, MBeanException;
    public TabularData getEnvironmentVariables() throws OpenDataException;
    public void setEnvironmentVariables(TabularData val) throws InvalidAttributeValueException, OpenDataException, MBeanException;
    public Map retrieveEnvVariablesMap();
    public void updateEnvVariablesMap(Map val) throws MBeanException;
    //Optional Configuration Schema
    /*With the requirement to change component configuration from a Web Browser, or from a thick-client GUI, it would be nice 
     *to provide validation of the values being changed, values that are secret like passwords not shown, detailed descriptions 
     *of configuration fields, and descriptive displays of configuration parameter labels. Since this information cannot be 
     *obtained in a detailed fashion from Standard MBeans that may be used to create the Configuration MBeans, it is possible 
     *to provide these details in other ways that can be used by the UI to create appropriate renderers and Validators. 
     *If component developers want to leverage these features, here is what needs to be done.
     *The component developer has to provide a schema corresponding to the attributes that are available in their 
     *Configuration MBean, and xml data that correspond to detailed descriptions of the attributes. Based on this, the UI 
     *can create appropriate renderers and Validators.
     */
    
    /* The retrieveConfigurationDisplaySchema operation returns the schema (defined in XSD) of the attributes 
     * that the Component Config MBean exposes. The schema can define restrictions and thus allows the developer to 
     * specify Enumerated Strings (which may be displayed as DropDowns in the UI) or restrict the integer fields to 
     * positive integers, specify totalDigits, and minInclusive or maxExclusive � in effect any definition that can 
     * be expressed in XSD can be placed on these attributes.
     */
    public String retrieveConfigurationDisplaySchema();
    
    /*The retrieveConfigurationDisplayData operation returns the XML data corresponding to the schema (defined in 
     * XSD) of the attributes that the Component Config MBean exposes. It is here that the component developer can specify 
     * descriptive names for the attributes that can appear in label fields of the UI, descriptions of the fields that can appear in 
     *ToolTips, or whether the field is a secret field (like a password field) or not so the UI can hide the actual data the user be
     *allowed to see from the UI. 
     */
    public String retrieveConfigurationDisplayData();
}