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
 * @(#)ForEachDBO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo;

import com.sun.jbi.engine.bpel.core.bpel.persist.DBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;


/** 
 * Defines interface for ForEach database object.
 * @author ksimpson
 */
public interface ForEachDBO extends DBObject {
    /** insert statement */
    String BASE_INSERT_STMT_STR = "INSERT INTO " + PersistenceDBSchemaCreation.FOREACH + //$NON-NLS-1$
        "VALUES(?, ?, ?, ?, ?, ?, ?)"; //$NON-NLS-1$

    /** update statement */
    String BASE_UPDATE_STMT_STR = "UPDATE " + PersistenceDBSchemaCreation.FOREACH + //$NON-NLS-1$
        "SET counter = ?, successes = ?, " + //$NON-NLS-1$  
        "startcount = ?, finalcount = ?, completioncount = ? " + //$NON-NLS-1$
        "WHERE stateid = ? AND foreachid = ?"; //$NON-NLS-1$ 

    /** delete statement */
    String BASE_DELETE_STMT_STR = "DELETE FROM " + PersistenceDBSchemaCreation.FOREACH + //$NON-NLS-1$
        "WHERE stateid = ? AND foreachid = ?"; //$NON-NLS-1$

    /** query statement */
    String BASE_QUERY_STMT_STR = "SELECT * FROM " + PersistenceDBSchemaCreation.FOREACH + //$NON-NLS-1$
        "WHERE stateid = ?"; //$NON-NLS-1$
    // TODO should query be ordered by Index column?
    
    /* sybase, db2, sqlserver, pointbase statements to be added */

    /**
     * get BPID
     *
     * @return String BPID
     */
    String getBPId();

    /**
     * get foreach ID
     *
     * @return long foreach iD
     */
    long getForEachId();

    /**
     * get iteration index
     *
     * @return iteration index
     */
    int getCounter();

    /**
     * get number of successful iterations
     * @return number of successful iterations
     */
    int getSuccesses();
    
    int getStartCount();
    
    int getFinalCount();
    
    int getCompletionCount();
}
