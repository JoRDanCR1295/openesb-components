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
 * @(#)VariableDBO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo;


import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;


/** @author Sun Microsystems
 * Mar 3, 2005
 */
public interface VariableDBO extends DBObject {
    
    /** insert statement */
    String BASE_INSERT_STMT_STR = "INSERT INTO " + PersistenceDBSchemaCreation.VARIABLE + //$NON-NLS-1$
        " VALUES (?, ?, '" + VariableScalabilityDBO.SCALABILITYPASSIVATED_NO + "', ?, ?)"; //$NON-NLS-1$
    
    /** update statement */
    String BASE_UPDATE_STMT_STR = "UPDATE " + PersistenceDBSchemaCreation.VARIABLE + //$NON-NLS-1$
        " SET value = ? " + "WHERE stateid = ? AND varid = ? AND scalabilitypassivated = '" + VariableScalabilityDBO.SCALABILITYPASSIVATED_NO + "' AND scopeguid = ?"; //$NON-NLS-1$ //$NON-NLS-2$

    /** delete statement */
    String BASE_DELETE_STMT_STR = "DELETE FROM " + PersistenceDBSchemaCreation.VARIABLE + //$NON-NLS-1$
    	" WHERE stateid = ? AND varid = ? AND scalabilitypassivated =  '" + VariableScalabilityDBO.SCALABILITYPASSIVATED_NO + "' AND scopeguid = ?"; //$NON-NLS-1$

    /** query statement */
    String BASE_QUERY_STMT_STR = "SELECT * FROM " + PersistenceDBSchemaCreation.VARIABLE + //$NON-NLS-1$
        " WHERE stateid = ?  and scalabilitypassivated =  '" + VariableScalabilityDBO.SCALABILITYPASSIVATED_NO + "'"; //$NON-NLS-1$
    
    String SCALABILITY_QUERY_STMT_STR = "SELECT * FROM " + PersistenceDBSchemaCreation.VARIABLE + //$NON-NLS-1$
		" WHERE stateid = ?  AND varid = ? AND scopeguid = ? AND scalabilitypassivated = '" + VariableScalabilityDBO.SCALABILITYPASSIVATED_NO + "'"; //$NON-NLS-1$    

    
    
    /** insert statement for simple type vars */
    String SIMPLE_BASE_INSERT_STMT_STR = "INSERT INTO " + PersistenceDBSchemaCreation.SIMPLEVARIABLE + //$NON-NLS-1$
    "(stateid, varid, stringvalue, scopeguid) values (?, ?, ?, ?)"; //$NON-NLS-1$

    /** update statement for simple type vars */
    String SIMPLE_BASE_UPDATE_STMT_STR = "UPDATE " + PersistenceDBSchemaCreation.SIMPLEVARIABLE + //$NON-NLS-1$
        "SET stringvalue = ? WHERE stateid = ? AND varid = ? AND scopeguid = ?"; //$NON-NLS-1$ //$NON-NLS-2$

    /** delete statement for simple type vars */
    String SIMPLE_BASE_DELETE_STMT_STR = "DELETE FROM " + PersistenceDBSchemaCreation.SIMPLEVARIABLE + //$NON-NLS-1$
        "WHERE stateid = ? AND varid = ? AND scopeguid = ?"; //$NON-NLS-1$

    /** query statement for simple type vars */
    String SIMPLE_BASE_QUERY_STMT_STR = "SELECT * FROM " + PersistenceDBSchemaCreation.SIMPLEVARIABLE + //$NON-NLS-1$
        "WHERE stateid = ?"; //$NON-NLS-1$

    /* sybase, db2, sqlserver, pointbase statements to be added */

    /**
     * get BPID
     *
     * @return String BPID
     */
    String getBPId();

    /**
     * get variable ID
     *
     * @return long variable iD
     */
    long getId();

    /**
     * get value
     *
     * @return Reader value
     */
    Object getValue();
    
    /**
     * get the value of the scopeId that this 
     * variable is associated with.
     * @return String scope Id.
     */
    String getScopeGuid();
    
}
