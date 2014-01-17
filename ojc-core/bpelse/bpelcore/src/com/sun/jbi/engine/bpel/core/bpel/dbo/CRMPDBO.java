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
 * @(#)CRMPDBO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo;

import java.io.Reader;

import com.sun.jbi.engine.bpel.core.bpel.persist.DBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public interface CRMPDBO extends DBObject {
    /* oracle statements */

    /** insert statement: insert the following columns
     * (bpid, crmpinvokeid, partnerlink, operation)
     * bpelmessageexchange may need to be addressed in the future..
     */
    String BASE_INSERT_STMT_STR = "INSERT INTO " + //$NON-NLS-1$
        PersistenceDBSchemaCreation.CRMP + " (stateid, crmpinvokeid, partnerlink, operation, bpelmessageexchange) VALUES(?, ?, ?, ?, ?)"; //$NON-NLS-1$

    /** update statement */
    String BASE_UPDATE_STMT_STR = "UPDATE " + //$NON-NLS-1$
    PersistenceDBSchemaCreation.CRMP + "SET replyvariableid = ?, responseobj = ? " + //$NON-NLS-1$
        "WHERE stateid = ? AND partnerlink = ? AND operation = ?"; //$NON-NLS-1$

    /** delete statement */
    String BASE_DELETE_STMT_STR = "DELETE FROM " + //$NON-NLS-1$
    PersistenceDBSchemaCreation.CRMP + "WHERE crmpinvokeid = ?"; //$NON-NLS-1$

    /** query statement */
    String BASE_QUERY_STMT_STR = "SELECT stateid, partnerlink, operation, replyvariableid, bpelmessageexchange FROM " + //$NON-NLS-1$
    PersistenceDBSchemaCreation.CRMP + "WHERE crmpinvokeid = ?"; //$NON-NLS-1$

    /** query statement */
    String QUERY_STMT_FOR_CLUSTERED_INVOKE_STR = "SELECT stateid, partnerlink, operation, replyvariableid, bpelmessageexchange FROM " + //$NON-NLS-1$
    PersistenceDBSchemaCreation.CRMP + " WHERE crmpinvokeid = ? and replyvariableid <> -1"; //$NON-NLS-1$

    /* sybase, db2, sqlserver, pointbase statements to be added */

    /**
     * The business process instance id.
     *
     * @return String id
     */
    String getBPId();

    /**
     * The response object value if present.
     *
     * @return Clob value
     */
    Reader getResponseObj();
    
    /**
     * The unique variable id of the reply activity.
     *
     * @return String value
     */
    long getReplyVariableId();

    /**
     * The unique crmpInvokeId
     *
     * @return a <code>String</code> crmpInvokeId vlaue.
     */
    String getCRMPInvokeId();
    
    /**
     * The Partnerlink of the message exchange
     *
     * @return a <code>String</code> partnerlink vlaue.
     */
    public String getPartnerLink();
    
    /**
     * The operation name of the porttype associated wiht the  message exchange
     * @return a <code>String</code> operation value
     */
    public String getOperation() ;
    
    /**
     * The bpel messageexchange associated with the receiving activity.
     * @return a <code>String</code> BPEL message exchange value
     */
    public String getBpelMessageExchange();
    
}
