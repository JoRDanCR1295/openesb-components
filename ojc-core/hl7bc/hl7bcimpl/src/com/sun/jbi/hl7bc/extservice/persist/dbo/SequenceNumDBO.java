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
 * @(#)SequenceNumDBO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.persist.dbo;

import com.sun.jbi.hl7bc.extservice.persist.DBSchemaCreation;

/**
 *
 * @author TVA Raghunadh
 */
public interface SequenceNumDBO extends DBObject {

	String SCHEMA_NAME = "HL7BC_SCHEMA";
    String TABLE_NAME_EXPECTEDSEQNO = "EXPSEQUENCENO";

	// FOR DERBY DATABASE

    /** insert statement */
    String BASE_INSERT_STMT_STR = "INSERT INTO " + DBSchemaCreation.EXPSEQUENCENO + //$NON-NLS-1$
            " VALUES(?, ?, ?)"; //$NON-NLS-1$

    /** update statement */
    String BASE_UPDATE_STMT_STR = "UPDATE " + DBSchemaCreation.EXPSEQUENCENO + //$NON-NLS-1$
            " SET ESN = ?, ESNSTATE = ?" + " WHERE ID =?"; //$NON-NLS-1$ //$NON-NLS-2$

    /** delete statement */
    String BASE_DELETE_STMT_STR = "DELETE FROM " + DBSchemaCreation.EXPSEQUENCENO + //$NON-NLS-1$
            " WHERE ESN = ?"; //$NON-NLS-1$

    /** query statement */
    String BASE_QUERY_STMT_STR = "SELECT * FROM " + DBSchemaCreation.EXPSEQUENCENO + //$NON-NLS-1$
            " WHERE ID = ?"; //$NON-NLS-1$

    /* sybase, db2, sqlserver, pointbase statements to be added */

	// FOR axion DATABASE

    /** insert statement */
    String BASE_INSERT_STMT_STR_AXIONDB = "INSERT INTO " + TABLE_NAME_EXPECTEDSEQNO + //$NON-NLS-1$
            " VALUES(?, ?, ?)"; //$NON-NLS-1$

    /** update statement */
    String BASE_UPDATE_STMT_STR_AXIONDB = "UPDATE " + TABLE_NAME_EXPECTEDSEQNO + //$NON-NLS-1$
            " SET ESN = ?, ESNSTATE = ?" + " WHERE ID =?"; //$NON-NLS-1$ //$NON-NLS-2$

    /** delete statement */
    String BASE_DELETE_STMT_STR_AXIONDB = "DELETE FROM " + TABLE_NAME_EXPECTEDSEQNO + //$NON-NLS-1$
            " WHERE ESN = ?"; //$NON-NLS-1$

    /** query statement */
    String BASE_QUERY_STMT_STR_AXIONDB = "SELECT * FROM " + TABLE_NAME_EXPECTEDSEQNO + //$NON-NLS-1$
            " WHERE ID = ?"; //$NON-NLS-1$



    /**
     * get Expected Sequence Number
     *
     * @return String Sequence Number
     */
    int getESN();

    void setESN(int val);

    /**
     * get Exepected Sequence Number State
     *
     * @return String ESN State
     */
    String getESNState();

    void setESNState(String val);

    /**
     * get Querying Key (unique ID) for record in EXPSEQUENCENO Table
     *
     * @return String VersionID-MsgType-Event
     */

    String getQueryingKey();

    void setQueryingKey(String val);

}
