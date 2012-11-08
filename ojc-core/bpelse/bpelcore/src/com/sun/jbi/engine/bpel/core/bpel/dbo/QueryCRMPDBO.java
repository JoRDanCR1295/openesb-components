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
 * @(#)QueryCRMPDBO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo;

import com.sun.jbi.engine.bpel.core.bpel.persist.DBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;

/**
 * This DBO exists to have a specific query on the CRMP table. The framework does not have the ability to define
 * multiple queries on the same dbo object. 
 * this DBO is used only to retrieve the entries from the CRMP table that does not have a response object column entry
 * for this BP id being recovered. 
 * Is a high level decision to minimize the database querys whereever possible, in view of which this DBO returns the set
 * of values in the CRMP table that need to be included in the update of these values when a reply activity completes
 * and persists its state. 
 *
 */
public interface QueryCRMPDBO extends DBObject{
	
    /** query statement */
    String BASE_QUERY_STMT_STR = "SELECT * FROM " + //$NON-NLS-1$
    PersistenceDBSchemaCreation.CRMP + "WHERE replyvariableid = -1 AND stateid = ?"; //$NON-NLS-1$	
    
    String getPartnerLink();
    
    String getOperation();
    
    String getBpelMessageExchange();
    
	String getBPId();
	
	long getReplyVariableId();

}
