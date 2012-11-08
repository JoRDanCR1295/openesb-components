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
 * @(#)LastCheckPointDBO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo;

import java.sql.Timestamp;

import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public interface LastCheckPointDBO extends DBObject {
    /* oracle statements */

    /** insert statement */
    String BASE_INSERT_STMT_STR = "INSERT INTO " + //$NON-NLS-1$
    PersistenceDBSchemaCreation.LASTCHECKPOINT + "VALUES(?, ?, ?, ? , ?)"; //$NON-NLS-1$

    /** update statement */
    String BASE_UPDATE_STMT_STR = "UPDATE " + PersistenceDBSchemaCreation.LASTCHECKPOINT + //$NON-NLS-1$
      "SET activityid = ?, " + "    timeval = ?, " +  //$NON-NLS-1$ //$NON-NLS-2$ 
        "    pickcompositeactid = ? " + //$NON-NLS-1$ 
        "WHERE (stateid = ? AND activityid = ?)"; //$NON-NLS-1$ //$NON-NLS-2$

    /** update statement */
    String BASE_UPDATE_STMT_STR_WITH_BRANCHINVOKEID = "UPDATE " + PersistenceDBSchemaCreation.LASTCHECKPOINT + //$NON-NLS-1$
      "SET activityid = ?, " + "    timeval = ?, " +  //$NON-NLS-1$ //$NON-NLS-2$ 
        "    pickcompositeactid = ?, " + "    branchinvokecounter = ?  " + //$NON-NLS-1$ //$NON-NLS-2$
        "WHERE (stateid = ? AND activityid = ?)"; //$NON-NLS-1$ //$NON-NLS-2$

    /** delete statement */
    String BASE_DELETE_STMT_STR = "DELETE FROM " + //$NON-NLS-1$
    PersistenceDBSchemaCreation.LASTCHECKPOINT + "WHERE (stateid = ? AND activityid = ?)"; //$NON-NLS-1$

    /** query statement */
    String BASE_QUERY_STMT_STR = "SELECT * FROM " + //$NON-NLS-1$
    PersistenceDBSchemaCreation.LASTCHECKPOINT + "WHERE stateid = ?"; //$NON-NLS-1$

    /* sybase, db2, sqlserver, pointbase statements to be added */

    /** default old program counter value */
    long DEFUALT_OLD_PC_VAL = -1;

    /** default faulted condition */
    char DEFUALT_FAULTED_CONDITION = 'N';

    /** faulted */
    char HAS_FAULTED = 'Y';

    /** default pick composite activity id */
    long DEFAULT_PICK_COMPOSITE_ACT_ID = 0;
    
    /** default branch invoke counter value */    
    long DEFAULT_BRANCH_INVOKE_COUNTER = 0;

    /**
     * DOCUMENT ME!
     *
     * @return String BPID
     */
    String getBPId();

    /**
     * DOCUMENT ME!
     *
     * @return the id that represents the current persistent point
     */
    long getActivityId();

    /**
     * DOCUMENT ME!
     *
     * @return id that represents the activity id that is being replaced by the  current activity
     *         Id
     */
    long getOldActivityId();

    /**
     * returns the timer value in java.sql.Date of an activity like Wait or OnAlarm.
     *
     * @return timer time stampe
     */
    Timestamp getTimerValue();

    /**
     * DOCUMENT ME!
     *
     * @return OnMessage or OnAlarm Id if present or null
     */
    long getPickCompositeActId();
    
    
    /**
     * DOCUMENT ME!
     *
     * @return the persisted branch invoke counter value
     */
    long getBranchInvokeCounter();
}
