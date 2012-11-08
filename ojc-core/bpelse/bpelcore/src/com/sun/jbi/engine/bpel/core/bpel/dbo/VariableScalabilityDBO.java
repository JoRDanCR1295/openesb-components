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
public interface VariableScalabilityDBO extends DBObject {
    
    public static final String SCALABILITYPASSIVATED_NO = "N";
    public static final String SCALABILITYPASSIVATED_YES = "Y";
    
    public static final int IS_COPY = 1;
    public static final int IS_UPDATE = 2;
    public static final int IS_READ = 3;
    public static final int IS_DELTE = 4;
    
    
    /** insert statement */
    String INSERT_PASSIVATED_VARIABLE_STMT_STR = "INSERT INTO " + PersistenceDBSchemaCreation.VARIABLE + //$NON-NLS-1$
        " VALUES (?, ?, '" + SCALABILITYPASSIVATED_YES + "', ?, ?)"; //$NON-NLS-1$
    
    String UPDATE_PASSIVATED_VAR_SCALABILITY_FLAG_STMT_STR = "UPDATE " + PersistenceDBSchemaCreation.VARIABLE + //$NON-NLS-1$
    	" SET scalabilitypassivated = '" + SCALABILITYPASSIVATED_NO + "' WHERE stateid = ? AND varid = ? AND scopeguid = ?"; //$NON-NLS-1$ //$NON-NLS-2$
    
    /** delete statement */
    String DELETE_PASSIVATED_VAR_STMT_STR = "DELETE FROM " + PersistenceDBSchemaCreation.VARIABLE + //$NON-NLS-1$
    	" WHERE stateid = ? AND varid = ? AND scalabilitypassivated = '" + SCALABILITYPASSIVATED_YES + "' AND scopeguid = ?"; //$NON-NLS-1$
    
    String DELETE_ALL_PASSIVATED_VARS_STMT_STR = "DELETE FROM " + PersistenceDBSchemaCreation.VARIABLE + //$NON-NLS-1$
    	" WHERE stateid = ?  AND scalabilitypassivated = '" + SCALABILITYPASSIVATED_YES + "'"; //$NON-NLS-1$
    
    /** query statement */
    String QUERY_PASSIVATED_VAR_STMT_STR = "SELECT * FROM " + PersistenceDBSchemaCreation.VARIABLE + //$NON-NLS-1$
    	" WHERE stateid = ?  AND varid = ? AND scopeguid = ? AND scalabilitypassivated = '" + SCALABILITYPASSIVATED_YES + "'"; //$NON-NLS-1$    
    
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
    
    String getScalabilityPassivatedFlag();
    
}
