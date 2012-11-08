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
 * @(#)RepUser.java
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
public class RepUser extends AbstractConfigTable {
    private String mUserId;
    private String mUserDesc;
    private String mUserName;
    private String mUserPassword;
    
    public RepUser() {
        super();
    }
    
    /**
     * @return the mUserDesc
     */
    public String getUserDesc() {
        return mUserDesc;
    }

    /**
     * @param userDesc the mUserDesc to set
     */
    public void setUserDesc(String userDesc) {
        mUserDesc = userDesc;
    }

    /**
     * @return the mUserId
     */
    public String getUserId() {
        return mUserId;
    }

    /**
     * @param userId the mUserId to set
     */
    public void setUserId(String userId) {
        mUserId = userId;
    }

    /**
     * @return the mUserName
     */
    public String getUserName() {
        return mUserName;
    }

    /**
     * @param userName the mUserName to set
     */
    public void setUserName(String userName) {
        mUserName = userName;
    }

    /**
     * @return the mUserPassword
     */
    public String getUserPassword() {
        return mUserPassword;
    }

    /**
     * @param userPassword the mUserPassword to set
     */
    public void setUserPassword(String userPassword) {
        mUserPassword = userPassword;
    }
    
}
