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
 * @(#)FTPTransferExtension.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc.extensions;

/**
 *
 * @author jfu
 */
public interface FTPTransferExtension {

    public static final String FTP_USE_PROPERTY = "use";
    public static final String FTP_ENCODINGSTYLE_PROPERTY = "encodingStyle";
    public static final String FTP_PART_PROPERTY = "part";
    public static final String FTP_CHARENCODING_PROPERTY = "characterEncoding";
    public static final String FTP_FILETYPE_PROPERTY = "fileType";
    public static final String FTP_FORWARD_AS_ATT_PROPERTY = "forwardAsAttachment";
    public static final String FTP_MSG_CORRELATE_ENABLED_PROPERTY = "messageCorrelate";
    public static final String FTP_POLLINTERVAL_PROPERTY = "pollIntervalMillis";
    public static final String FTP_SYMETRIC_MSG_NAME_PROPERTY = "messageName";
    public static final String FTP_SYMETRIC_MSG_NAME_PREFIX_IB_PROPERTY = "messageNamePrefixIB";
    public static final String FTP_SYMETRIC_MSG_NAME_PREFIX_OB_PROPERTY = "messageNamePrefixOB";

    public String getPart();

    public void setPart(String s);

    public String getEncodingStyle();

    public void setEncodingStyle(String s);

    public String getUse();

    public void setUse(String s);

    public int getPollInterval();

    public void setPollInterval(int interval);

    public boolean getMessageCorrelate();

    public void setMessageCorrelate(boolean b);

    public void validate(String s) throws Exception;

    public Object[] getAttributes();

    public String getMessageName();

    public void setMessageName(String s);

    public String getMessageNamePrefixIB();

    public void setMessageNamePrefixIB(String s);

    public String getMessageNamePrefixOB();

    public void setMessageNamePrefixOB(String s);

    public String getFileType();

    public void setFileType(String s);

    public String getCharacterEncoding();

    public void setCharacterEncoding(String s);

    public boolean getForwardAsAttachment();

    public void setForwardAsAttachment(boolean b);
}
