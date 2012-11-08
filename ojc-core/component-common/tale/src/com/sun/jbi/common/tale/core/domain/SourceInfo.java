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
 * @(#)SourceInfo.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.domain;

import java.sql.Timestamp;

/**
 * 
 * @author Kevan Simpson
 */
public class SourceInfo {
    private String mProjectName;        // optional, max length 128
    private String mApplicationType;    // optional, max length 128
    private String mApplicationName;    // optional, max length 128
    private String mServiceName;        // optional, max length 128
    private String mModuleName;         // optional, max length 128
    private String mUnitName;           // optional, max length 128
    private Timestamp mDateTimeStamp;   // optional, nillable xsd:dateTime
    private String mAppMessageID;          // optional, max length 128
    
    private String mInstanceName;
    private String mComponentType;
    private String mComponentName;
    
    public static SourceInfo copy(SourceInfo info) {
        SourceInfo copy = null;
        if (info != null) {
            copy = new SourceInfo();
            copy.setApplicationName(info.getApplicationName());
            copy.setApplicationType(info.getApplicationType());
            copy.setDateTimeStamp(info.getDateTimeStamp());
            copy.setAppMessageID(info.getAppMessageID());
            copy.setModuleName(info.getModuleName());
            copy.setProjectName(info.getProjectName());
            copy.setServiceName(info.getServiceName());
            copy.setUnitName(info.getUnitName());
            copy.setInstanceName(info.getInstanceName());
            copy.setComponentName(info.getComponentName());
            copy.setComponentType(info.getComponentType());
        }
        return copy;
    }

    /**
     * @return the projectName
     */
    public String getProjectName() {
        return mProjectName;
    }
    /**
     * @param projectName the projectName to set
     */
    public void setProjectName(String projectName) {
        mProjectName = projectName;
    }
    /**
     * @return the applicationType
     */
    public String getApplicationType() {
        return mApplicationType;
    }
    /**
     * @param applicationType the applicationType to set
     */
    public void setApplicationType(String applicationType) {
        mApplicationType = applicationType;
    }
    /**
     * @return the applicationName
     */
    public String getApplicationName() {
        return mApplicationName;
    }
    /**
     * @param applicationName the applicationName to set
     */
    public void setApplicationName(String applicationName) {
        mApplicationName = applicationName;
    }
    /**
     * @return the serviceName
     */
    public String getServiceName() {
        return mServiceName;
    }
    /**
     * @param serviceName the serviceName to set
     */
    public void setServiceName(String serviceName) {
        mServiceName = serviceName;
    }
    /**
     * @return the moduleName
     */
    public String getModuleName() {
        return mModuleName;
    }
    /**
     * @param moduleName the moduleName to set
     */
    public void setModuleName(String moduleName) {
        mModuleName = moduleName;
    }
    /**
     * @return the unitName
     */
    public String getUnitName() {
        return mUnitName;
    }
    /**
     * @param unitName the unitName to set
     */
    public void setUnitName(String unitName) {
        mUnitName = unitName;
    }
    /**
     * @return the dateTimeStamp
     */
    public Timestamp getDateTimeStamp() {
        return mDateTimeStamp;
    }
    /**
     * @param dateTimeStamp the dateTimeStamp to set
     */
    public void setDateTimeStamp(Timestamp dateTimeStamp) {
        mDateTimeStamp = dateTimeStamp;
    }
    /**
     * @return the messageID
     */
    public String getAppMessageID() {
        return mAppMessageID;
    }
    /**
     * @param messageID the messageID to set
     */
    public void setAppMessageID(String messageID) {
        mAppMessageID = messageID;
    }

    /**
     * @return the mComponentName
     */
    public String getComponentName() {
        return mComponentName;
    }

    /**
     * @param componentName the mComponentName to set
     */
    public void setComponentName(String componentName) {
        mComponentName = componentName;
    }

    /**
     * @return the mComponentType
     */
    public String getComponentType() {
        return mComponentType;
    }

    /**
     * @param componentType the mComponentType to set
     */
    public void setComponentType(String componentType) {
        mComponentType = componentType;
    }


    /**
     * @return the mInstanceName
     */
    public String getInstanceName() {
        return mInstanceName;
    }

    /**
     * @param instanceName the mInstanceName to set
     */
    public void setInstanceName(String instanceName) {
        mInstanceName = instanceName;
    }
}
