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
 * @(#)FTPMessageExtension.java 
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
public interface FTPMessageExtension extends FTPTransferExtension {

    /**
     * 
     */
    public static final String FTP_MSG_REPO_PROPERTY = "messageRepository";
    /**
     * 
     */
    public static final String FTP_PROTECT_ENABLED_PROPERTY = "protect";
    /**
     * 
     */
    public static final String FTP_ARCHIVE_ENABLED_PROPERTY = "archive";
    /**
     * 
     */
    public static final String FTP_STAGING_ENABLED_PROPERTY = "stage";

    /**
     * 
     * @return 
     */
    public String getMessageRepo();

    /**
     * 
     * @param s 
     */
    public void setMessageRepo(String s);

    /**
     * 
     * @return 
     */
    public boolean getProtectEnabled();

    /**
     * 
     * @param b 
     */
    public void setProtectEnabled(boolean b);

    /**
     * 
     * @return 
     */
    public boolean getArchiveEnabled();

    /**
     * 
     * @param b 
     */
    public void setArchiveEnabled(boolean b);

    /**
     * 
     * @return 
     */
    public boolean getStagingEnabled();

    /**
     * 
     * @param b 
     */
    public void setStagingEnabled(boolean b);
}
