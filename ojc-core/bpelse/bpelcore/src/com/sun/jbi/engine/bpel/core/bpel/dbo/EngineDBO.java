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
 * @(#)EngineDBO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo;

import com.sun.jbi.engine.bpel.core.bpel.persist.DBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public interface EngineDBO extends DBObject {
    /* oracle statements */

    /** insert statement */
    //String BASE_INSERT_STMT_STR = "INSERT INTO " + DBSchemaCreation.ENGINE + //$NON-NLS-1$
    //    "VALUES(?, ?, ?)"; //$NON-NLS-1$
    String BASE_INSERT_STMT_STR = "INSERT INTO " + PersistenceDBSchemaCreation.ENGINE + //$NON-NLS-1$
    "VALUES(?, ?, CURRENT_TIMESTAMP)"; //$NON-NLS-1$ //$NON-NLS-1$

    /** update statement */
    //String BASE_PASSIVATION_UPDATE_STMT_STR = "UPDATE " + DBSchemaCreation.ENGINE + //$NON-NLS-1$
    //    "SET expiration = ?" + "WHERE id = ?"; //$NON-NLS-1$ //$NON-NLS-2$
    
    String BASE_UPDATE_STMT_STR = "UPDATE " + PersistenceDBSchemaCreation.ENGINE + //$NON-NLS-1$
    "SET lastupdatetime = CURRENT_TIMESTAMP " + "WHERE engineid = ?"; //$NON-NLS-1$ //$NON-NLS-2$

    /** delete statement */
    String BASE_DELETE_STMT_STR = "DELETE FROM " + PersistenceDBSchemaCreation.ENGINE + //$NON-NLS-1$
        "WHERE engineid = ?"; //$NON-NLS-1$

    /** query statement */
    String BASE_QUERY_STMT_STR = "SELECT * FROM " + PersistenceDBSchemaCreation.ENGINE + //$NON-NLS-1$
        "WHERE engineid = ?"; //$NON-NLS-1$

    /* oracle specific SQLs */
    String ORCL_INSERT_STMT_STR = "INSERT INTO " + PersistenceDBSchemaCreation.ENGINE + " VALUES(?, ?, sysdate)"; //$NON-NLS-1$ //$NON-NLS-1$ //$NON-NLS-1$
    String ORCL_UPDATE_STMT_STR = "UPDATE " + PersistenceDBSchemaCreation.ENGINE + " SET lastupdatetime = sysdate " + "WHERE engineid = ? ";//$NON-NLS-1$ //$NON-NLS-1$ //$NON-NLS-1$  
    
    /* postgresql specific SQLs */
    String PGSQL_INSERT_STMT_STR = "INSERT INTO " + PersistenceDBSchemaCreation.ENGINE + " VALUES(?, ?, now())"; //$NON-NLS-1$ //$NON-NLS-1$ //$NON-NLS-1$
    String PGSQL_UPDATE_STMT_STR = "UPDATE " + PersistenceDBSchemaCreation.ENGINE + " SET lastupdatetime = now() " + "WHERE engineid = ? ";//$NON-NLS-1$ //$NON-NLS-1$ //$NON-NLS-1$


    /**
     * DOCUMENT ME!
     *
     * @return String engine ID
     */
    String getId();

    /**
     * DOCUMENT ME!
     *
     * @return String engine location
     */
    String getLocation();

    /**
     * DOCUMENT ME!
     *
     * @return long engine lease expiration
     */
    long getExpiration();
}
