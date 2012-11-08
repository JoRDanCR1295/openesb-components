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
 * @(#)QueryRespCRMPDBO.java 
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
 * This DBO exists to have a specific query on the CRMP table. The framework does not have the ability to define
 * multiple queries on the same dbo object. 
 * CR #  .CRMP optimization for performance changes. This query would only be used in the SubBP recovery scenario
 * where Response needs to be send to the calling MainBP when there are no in-memory references to the response 
 * object. this would be a recovery scenario where the SubBP had persisted the response obj in the crmp table and
 * proceeded after the reply activity, before the crash occured. 
 */
public interface QueryRespCRMPDBO extends DBObject{
	
    /** query statement */
    String BASE_QUERY_STMT_STR = "SELECT responseobj FROM " + //$NON-NLS-1$
    PersistenceDBSchemaCreation.CRMP + "WHERE crmpinvokeid = ?"; //$NON-NLS-1$	
    
    /**
     * The response object value if present.
     *
     * @return Clob value
     */
    Reader getResponseObj();
    
    /**
     * The CrmpInvokeId that is the input for the query.
     * @return
     */
    String getCRMPInvokeId();

}
