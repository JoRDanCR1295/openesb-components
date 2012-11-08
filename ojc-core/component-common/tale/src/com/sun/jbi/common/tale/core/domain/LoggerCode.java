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
 * @(#)LoggerCode.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.domain;

/**
 * 
 * @author Sun Microsystems
 */
public class LoggerCode extends AbstractConfigTable {
    private int mLoggerCode;

    private String mLoggerLabel;

    private String mLoggerCategory;

    private String mLoggerLevel;

    private String mLoggerDescription;

    private int mLoggerChannelCode;

    public LoggerCode() {
        super();
    }

    public LoggerCode(int loggerCode, String loggerLabel,
            String loggerCategory, String loggerLevel,
            String loggerDescription, int loggerChannelCode,
            boolean activeFlag, String createID, String modifiedID) {
        super();
        mLoggerCode = loggerCode;
        mLoggerLabel = loggerLabel;
        mLoggerCategory = loggerCategory;
        mLoggerLevel = loggerLevel;
        mLoggerDescription = loggerDescription;
        mLoggerChannelCode = loggerChannelCode;
    }

    /**
     * @return the mLoggerCategory
     */
    public String getLoggerCategory() {
        return mLoggerCategory;
    }

    /**
     * @param loggerCategory
     *            the mLoggerCategory to set
     */
    public void setLoggerCategory(String loggerCategory) {
        mLoggerCategory = loggerCategory;
    }

    /**
     * @return the mLoggerChannelCode
     */
    public int getLoggerChannelCode() {
        return mLoggerChannelCode;
    }

    /**
     * @param loggerChannelCode
     *            the mLoggerChannelCode to set
     */
    public void setLoggerChannelCode(int loggerChannelCode) {
        mLoggerChannelCode = loggerChannelCode;
    }

    /**
     * @return the mLoggerCode
     */
    public int getLoggerCode() {
        return mLoggerCode;
    }

    /**
     * @param loggerCode
     *            the mLoggerCode to set
     */
    public void setLoggerCode(int loggerCode) {
        mLoggerCode = loggerCode;
    }

    /**
     * @return the mLoggerDescription
     */
    public String getLoggerDescription() {
        return mLoggerDescription;
    }

    /**
     * @param loggerDescription
     *            the mLoggerDescription to set
     */
    public void setLoggerDescription(String loggerDescription) {
        mLoggerDescription = loggerDescription;
    }

    /**
     * @return the mLoggerLabel
     */
    public String getLoggerLabel() {
        return mLoggerLabel;
    }

    /**
     * @param loggerLabel
     *            the mLoggerLabel to set
     */
    public void setLoggerLabel(String loggerLabel) {
        mLoggerLabel = loggerLabel;
    }

    /**
     * @return the mLoggerLevel
     */
    public String getLoggerLevel() {
        return mLoggerLevel;
    }

    /**
     * @param loggerLevel
     *            the mLoggerLevel to set
     */
    public void setLoggerLevel(String loggerLevel) {
        mLoggerLevel = loggerLevel;
    }

}
