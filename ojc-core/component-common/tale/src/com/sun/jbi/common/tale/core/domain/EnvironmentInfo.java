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
 * @(#)EnvironmentInfo.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.domain;

/**
 * Domain Object used for storing Environment related fields in the 
 * CSF_CME_LOG table
 * @author Sun Microsystems
 */
public class EnvironmentInfo {
    private String mServerType;
    private String mServerName;
    private String mHostName;
    private String mLogicalHostName;
    private String mEnvName;
    
    public static EnvironmentInfo copy(EnvironmentInfo info) {
        EnvironmentInfo copy = null;
        if (info != null) {
            copy = new EnvironmentInfo();
            copy.setServerType(info.getServerType());
            copy.setServerName(info.getServerName());
            copy.setHostName(info.getHostName());
            copy.setLogicalHostName(info.getLogicalHostName());
            copy.setEnvName(info.getEnvName());
        }
        return copy;
    }

    /**
     * @return the mEnvName
     */
    public String getEnvName() {
        return mEnvName;
    }

    /**
     * @param envName the mEnvName to set
     */
    public void setEnvName(String envName) {
        mEnvName = envName;
    }

    /**
     * @return the mHostName
     */
    public String getHostName() {
        return mHostName;
    }

    /**
     * @param hostName the mHostName to set
     */
    public void setHostName(String hostName) {
        mHostName = hostName;
    }

    /**
     * @return the mLogicalHostName
     */
    public String getLogicalHostName() {
        return mLogicalHostName;
    }

    /**
     * @param logicalHostName the mLogicalHostName to set
     */
    public void setLogicalHostName(String logicalHostName) {
        mLogicalHostName = logicalHostName;
    }

    /**
     * @return the mServerName
     */
    public String getServerName() {
        return mServerName;
    }

    /**
     * @param serverName the mServerName to set
     */
    public void setServerName(String serverName) {
        mServerName = serverName;
    }

    /**
     * @return the mServerType
     */
    public String getServerType() {
        return mServerType;
    }

    /**
     * @param serverType the mServerType to set
     */
    public void setServerType(String serverType) {
        mServerType = serverType;
    }

}
