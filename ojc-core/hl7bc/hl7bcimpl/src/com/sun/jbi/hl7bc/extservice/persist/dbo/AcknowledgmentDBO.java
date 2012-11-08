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
 * @(#)AcknowledgmentDBO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.persist.dbo;

import com.sun.jbi.hl7bc.extservice.persist.DBSchemaCreation;

/**
 * @author S. Nageswara Rao
 */
public interface AcknowledgmentDBO extends DBObject {

    /** insert statement */
    String BASE_INSERT_STMT_STR = "INSERT INTO " + //$NON-NLS-1$
            DBSchemaCreation.ACKNOWLEDGMENT + "VALUES(?, ?)"; //$NON-NLS-1$

    /** update statement */
    String BASE_UPDATE_STMT_STR = "UPDATE " + //$NON-NLS-1$
            DBSchemaCreation.ACKNOWLEDGMENT + "SET value = ? " + //$NON-NLS-1$
            "WHERE mcid = ?"; //$NON-NLS-1$

    /** delete statement */
    String BASE_DELETE_STMT_STR = "DELETE FROM " + //$NON-NLS-1$
            DBSchemaCreation.ACKNOWLEDGMENT + "WHERE mcid = ?"; //$NON-NLS-1$

    /** query statement */
    String BASE_QUERY_STMT_STR = "SELECT * FROM " + //$NON-NLS-1$
            DBSchemaCreation.ACKNOWLEDGMENT + "WHERE mcid = ?"; //$NON-NLS-1$

    /* sybase, db2, sqlserver, pointbase statements to be added */

    /**
     * get Message Control ID
     * 
     * @return String messagecontrolID
     */
    String getmcid();

    /**
     * get Acknowledgment Message
     * 
     * @return String acknowledgment
     */
    String getMessage();

}
