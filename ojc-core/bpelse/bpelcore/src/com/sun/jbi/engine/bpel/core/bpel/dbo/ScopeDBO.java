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
 * @(#)FaultHandlerDataDBO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo;

import java.io.Reader;

import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;


/** @author Sun Microsystems
 * Mar 3, 2005
 */
public interface ScopeDBO extends DBObject {
    /** insert statement */
    String BASE_INSERT_STMT_STR = "INSERT INTO " + PersistenceDBSchemaCreation.SCOPE + //$NON-NLS-1$
        "VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"; //$NON-NLS-1$

    /** update statement */
    String BASE_UPDATE_STMT_STR = "UPDATE " + PersistenceDBSchemaCreation.SCOPE + //$NON-NLS-1$
        "SET parentscopeguid = ?, scopestate = ?, compensateid = ?, completionorder = ?, faultname = ?, faultdata = ?, faultactivityid = ? " + //$NON-NLS-1$  
        "WHERE stateid = ? AND scopeguid = ? AND scopeid = ?"; //$NON-NLS-1$ 

    /** delete statement */
    String BASE_DELETE_STMT_STR = "DELETE FROM " + PersistenceDBSchemaCreation.SCOPE + //$NON-NLS-1$
        "WHERE stateid = ? AND scopeguid = ? AND scopeid = ?"; //$NON-NLS-1$

    /** query statement */
    String BASE_QUERY_STMT_STR = "SELECT * FROM " + PersistenceDBSchemaCreation.SCOPE + //$NON-NLS-1$
        "WHERE stateid = ?"; //$NON-NLS-1$

    /* sybase, db2, sqlserver, pointbase statements to be added */

    /**
     * get BPID
     *
     * @return String BPID
     */
    String getBPId();

    long getScopeId();
    
    /**
     * get Scope ID that this fault belongs to
     *
     * @return long Scope iD
     */
    String getScopeGuid();

    /**
     * get fault data
     *
     * @return Reader
     */
    Reader getFaultData();
    
    /** get the fault name
     * @return QName
     */
    QName getFaultName();
    
    String getScopeState();
    
    String getParentScopeGuid();
    
    long getCompensateId();
    
    long getCompletionOrder();
    
    long getFaultActivityId();
}
