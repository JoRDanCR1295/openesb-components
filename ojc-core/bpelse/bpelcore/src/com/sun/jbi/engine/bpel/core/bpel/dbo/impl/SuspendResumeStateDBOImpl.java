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
 * @(#)$Id: SuspendResumeStateDBOImpl.java,v 1.1 2007/09/14 20:56:20 mei_wu Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo.impl;

import java.sql.PreparedStatement;
import java.sql.SQLException;

/**
 * The class for state update when a bpel instance is suspended
 * or resumed
 * 
 * @author Sun Microsystems
 *
 */
public class SuspendResumeStateDBOImpl extends StateDBOImpl {

	/**
	 * Created when suspend/resume occur to update the state in db
	 * @param dbType
	 * @param id
	 * @param suspend Whether the DBO is a suspend dbo or resume dbo
	 */
	public SuspendResumeStateDBOImpl(int dbType, String id, boolean suspend) {
		super(dbType, id);
		if (suspend) {
			init(null, SUSPEND_STMT_STR, null,
		            null);
		} else {
			init(null, RESUME_STMT_STR, null, null);
		}
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.dbo.impl.StateDBOImpl#fillUpdateStmt(java.sql.PreparedStatement)
	 */
	@Override
	public void fillUpdateStmt(PreparedStatement stmt) throws SQLException {
		// TODO Auto-generated method stub
		stmt.setString(1, getId());		
	}


}
