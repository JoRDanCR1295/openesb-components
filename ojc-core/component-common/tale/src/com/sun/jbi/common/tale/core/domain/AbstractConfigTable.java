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
 * @(#)AbstractConfigTable.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.domain;

import java.sql.Timestamp;

/**
 * An abstract class for all ALE Configuration Tables
 * @author Sun Microsystems
 */
public class AbstractConfigTable {
    /* active flag - indicates whether the current user/channel/code is active */
    private boolean mActiveFlag;
    /* User ID of the creator*/
    private String mCreateID;
    /* Timestamp indicating when the record was created */
    private Timestamp mCreateDateTime;
    /* ID of the user who modified the record */
    private String mModifiedID;
    /* Timestamp indicating when the record was last modified */
    private Timestamp mModifiedDateTime;
    
    public AbstractConfigTable() {
        super();
    }

    /**
     * @return the mActiveFlag
     */
    public boolean isActiveFlag() {
        return mActiveFlag;
    }

    /**
     * @param activeFlag the mActiveFlag to set
     */
    public void setActiveFlag(boolean activeFlag) {
        mActiveFlag = activeFlag;
    }

    /**
     * @return the mCreateDateTime
     */
    public Timestamp getCreateDateTime() {
        return mCreateDateTime;
    }

    /**
     * @param createDateTime the mCreateDateTime to set
     */
    public void setCreateDateTime(Timestamp createDateTime) {
        mCreateDateTime = createDateTime;
    }

    /**
     * @return the mCreateID
     */
    public String getCreateID() {
        return mCreateID;
    }

    /**
     * @param createID the mCreateID to set
     */
    public void setCreateID(String createID) {
        mCreateID = createID;
    }

    /**
     * @return the mModifiedDateTime
     */
    public Timestamp getModifiedDateTime() {
        return mModifiedDateTime;
    }

    /**
     * @param modifiedDateTime the mModifiedDateTime to set
     */
    public void setModifiedDateTime(Timestamp modifiedDateTime) {
        mModifiedDateTime = modifiedDateTime;
    }

    /**
     * @return the mModifiedID
     */
    public String getModifiedID() {
        return mModifiedID;
    }

    /**
     * @param modifiedID the mModifiedID to set
     */
    public void setModifiedID(String modifiedID) {
        mModifiedID = modifiedID;
    }
}
