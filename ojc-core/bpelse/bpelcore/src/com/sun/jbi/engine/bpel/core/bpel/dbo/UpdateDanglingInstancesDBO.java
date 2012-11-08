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
 * @(#)UpdateDanglingInstancesDBO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo;

import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;

public interface UpdateDanglingInstancesDBO extends DBObject {
    
	/*
	 * NOTE: The oracle equivalent of rownum is not available for derby, hence failover scalabillity support is not available for 
	 * Derby for now. Hence the query for Derby looks different from oracle.
	 */
    String BASE_UPDATE_STMT_STR = "update " + PersistenceDBSchemaCreation.STATE + " set engineid = ? " //$NON-NLS-1$ //$NON-NLS-1$ 
            + " where (status = '" + StateDBO.RUNNING_STATUS + "' or status = '" + StateDBO.SUSPENDED_STATUS
            + "') and engineid IN (select engineid from " + PersistenceDBSchemaCreation.ENGINE
            + " where {fn TIMESTAMPDIFF(SQL_TSI_SECOND, timestamp(lastupdatetime), CURRENT_TIMESTAMP)} > ? / 1000 " 
            + " and engineid != ?) ";  //$NON-NLS-1$ 
    
    String ORCL_UPDATE_STMT_STR = "update " + PersistenceDBSchemaCreation.STATE  + " set engineid = ? " //$NON-NLS-1$
    		+ " where (status = '" + StateDBO.RUNNING_STATUS + "' or status = '" + StateDBO.SUSPENDED_STATUS 
    		+ "') and engineid IN (select engineid from " + PersistenceDBSchemaCreation.ENGINE 
    		+ " where ((sysdate - lastupdatetime) * 86400)  > (? / 1000) " + " and engineid != ?) " //$NON-NLS-1$
    		+ " and rownum < (? + 1)"; //$NON-NLS-1$

    String PGSQL_UPDATE_STMT_STR = "update " + PersistenceDBSchemaCreation.STATE  + " set engineid = ? " //$NON-NLS-1$
    		+ " where (status = '" + StateDBO.RUNNING_STATUS + "' or status = '" + StateDBO.SUSPENDED_STATUS
    		+ "') and engineid IN (select engineid from " + PersistenceDBSchemaCreation.ENGINE
    		+ " where extract(EPOCH from (now() - lastupdatetime))  > (? / 1000) " + " and engineid != ?) "; //$NON-NLS-1$

}
