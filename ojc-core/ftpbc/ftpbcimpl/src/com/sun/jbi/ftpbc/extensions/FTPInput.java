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
 * @(#)FTPInput.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc.extensions;

import java.io.Serializable;

/**
 *
 * @author jfu
 */
public class FTPInput implements Serializable {
    // assume that <input> can only have at most one
    // child of type FTPTransferExtension
    // which is the common interface for
    // FTPTRansfer, FTPMessage, FTPMessageActivePassive

    private static final long serialVersionUID = 1L;
    private FTPTransferExtension mExtElem;

    /**
     * 
     * @return 
     */
    public FTPTransferExtension getExtension() {
        return mExtElem;
    }

    /**
     * 
     * @param extElem 
     */
    public void setExtension(FTPTransferExtension extElem) {
        mExtElem = extElem;
    }
}
