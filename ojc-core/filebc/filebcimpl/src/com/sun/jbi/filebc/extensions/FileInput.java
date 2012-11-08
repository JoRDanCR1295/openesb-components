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
 * @(#)FileInput.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.extensions;

import java.io.Serializable;

/**
 *
 * @author Sherry Weng
 */
public class FileInput implements Serializable {

    private static final long serialVersionUID = 1L;
    FileMessage mFileMessage = null;

    /**
     * Set the file:message element properties for File binding Input
     */
    public void setFileMessage(FileMessage message) {
        mFileMessage = message;
    }

    /**
     * Get the file:message element properties for File binding Input
     */
    public FileMessage getFileMessage() {
        return mFileMessage;
    }
}
