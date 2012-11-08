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
 * @(#)ValidationInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.extservice;

/**
 * @author T.V.A.Raghunadh
 */

public class ValidationInfo {

    private boolean mStatus = true;

    private String mErrorCode;

    private String mErrorMesage;

    /**
     * Set the Validation Status
     * 
     * @param val
     */

    public void setValidationStatus(boolean val) {
        this.mStatus = val;
    }

    /**
     * Get the Validation Status
     * 
     * @return mStatus, validation status
     */
    public boolean getValidationStatus() {
        return this.mStatus;
    }

    /**
     * Set the Error Code
     * 
     * @param val
     */

    public void setErrorCode(String val) {
        this.mErrorCode = val;
    }

    /**
     * Get the error Code
     * 
     * @return mErrorCode
     */
    public String getErrorCode() {
        return this.mErrorCode;
    }

    /**
     * Set the Error Message
     * 
     * @param val
     */
    public void setErrorMessage(String val) {
        this.mErrorMesage = val;
    }

    /**
     * Get the Error Message
     * 
     * @return mErrorMesage
     */
    public String getErrorMessage() {
        return this.mErrorMesage;
    }

}
