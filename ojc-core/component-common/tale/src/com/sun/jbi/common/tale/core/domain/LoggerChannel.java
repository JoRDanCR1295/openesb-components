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
 * @(#)LoggerChannel.java
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
public class LoggerChannel extends AbstractConfigTable {
    private int mLoggerChannelCode;
    private String mChannelType;
    private String mFileName;
    
    public LoggerChannel() {
		super();
	}

    /**
     * @return the mChannelType
     */
    public String getChannelType() {
        return mChannelType;
    }

    /**
     * @param channelType the mChannelType to set
     */
    public void setChannelType(String channelType) {
        mChannelType = channelType;
    }

    /**
     * @return the mFileName
     */
    public String getFileName() {
        return mFileName;
    }

    /**
     * @param fileName the mFileName to set
     */
    public void setFileName(String fileName) {
        mFileName = fileName;
    }

    /**
     * @return the mLoggerChannelCode
     */
    public int getLoggerChannelCode() {
        return mLoggerChannelCode;
    }

    /**
     * @param loggerChannelCode the mLoggerChannelCode to set
     */
    public void setLoggerChannelCode(int loggerChannelCode) {
        mLoggerChannelCode = loggerChannelCode;
    }

}
