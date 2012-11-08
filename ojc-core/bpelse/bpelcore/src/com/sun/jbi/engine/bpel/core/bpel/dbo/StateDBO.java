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
 * @(#)StateDBO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo;

import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.persist.DBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.State;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public interface StateDBO extends DBObject {
    /** engine running status */
    String RUNNING_STATUS = "RUNNING"; //$NON-NLS-1$
    
    /** instance suspended status */
    String SUSPENDED_STATUS = "SUSPENDED";

    /** Locks **/
    // lock to indicate the instance is executed by engine  
    String OWNERLOCK_YES = "Y"; //$NON-NLS-1$

    // lock to indicate the instance is passivated
    String OWNERLOCK_NO = "N"; //$NON-NLS-1$


    /** engine complete status */
    String COMPLETE_STATUS = "DONE"; //$NON-NLS-1$

    /** insert statement */
    String BASE_INSERT_STMT_STR = "INSERT INTO " + PersistenceDBSchemaCreation.STATE + //$NON-NLS-1$
        "(stateid, bpelid, engineid, status) VALUES (?, ?, ?, ?)"; //$NON-NLS-1$

    /** update statement */
    //String BASE_UPDATE_STMT_STR = ""; //$NON-NLS-1$
    //String BASE_UPDATE_STMT_STR = "UPDATE " + DBSchemaCreation.STATE +  " set OWNERLOCK = ? where STATUS = '" + RUNNING_STATUS +"' and OWNERLOCK = ? and stateid = ?"; //$NON-NLS-1$ //$NON-NLS-1$
    String BASE_UPDATE_STMT_STR = "UPDATE " + PersistenceDBSchemaCreation.STATE + " set engineid = ?, ownerlock = ? where (status = '" + StateDBO.RUNNING_STATUS + "' or status = '" + SUSPENDED_STATUS +"' ) and ownerlock = ? and stateid = ?";
    
    /**Suspend update statement */
    String SUSPEND_STMT_STR = "UPDATE " +  PersistenceDBSchemaCreation.STATE + " SET  status ='" + SUSPENDED_STATUS + "' WHERE  stateid = ?";
    
    /**Resume update statement */
    String RESUME_STMT_STR = "UPDATE " +  PersistenceDBSchemaCreation.STATE + " SET  status ='" + RUNNING_STATUS + "' WHERE  stateid = ?";

    //String ORCL_DELETE_STMT_STR = "delete from State where bpid = ?";

    /** delete statement */
    String BASE_DELETE_STMT_STR = "UPDATE " + PersistenceDBSchemaCreation.STATE + //$NON-NLS-1$
        "SET status = ? " + "WHERE stateid = ?"; //$NON-NLS-1$ //$NON-NLS-2$

    /** query statement */
    String BASE_QUERY_STMT_STR = "SELECT * FROM " + PersistenceDBSchemaCreation.STATE + //$NON-NLS-1$
        "WHERE stateid = ?"; //$NON-NLS-1$

    /* sybase, db2, sqlserver, pointbase statements to be added */

    /**
     * get state ID
     *
     * @return String state ID
     */
    String getId();

    /**
     * get BPEL ID
     *
     * @return QName BPEL ID
     */
    //String getBPELId();
    QName getBPELId();

    /**
     * get engine ID
     *
     * @return String engine id
     */
    String getEngineId();
    
    State.Mode getMode();
    
    String getStatus();
    
    String getOwnerLock();
}
