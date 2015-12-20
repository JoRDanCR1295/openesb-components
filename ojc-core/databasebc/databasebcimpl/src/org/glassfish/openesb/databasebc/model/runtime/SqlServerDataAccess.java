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
 * @(#)SqlServerDataAccess.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.model.runtime;

import com.sun.jbi.internationalization.Messages;
import java.util.logging.Level;

import java.util.logging.Logger;


/**
 * @author  Venkat P
 *
 */
public class SqlServerDataAccess implements DatabaseModel {
    private static final Messages mMessages = Messages.getMessages(SqlServerDataAccess.class);
    private static final Logger mLogger = Messages.getLogger(SqlServerDataAccess.class);

    private SqlServerDataAccess(){
    }

    private static final SqlServerDataAccess instance = new SqlServerDataAccess();
    public static final SqlServerDataAccess getInstance(){
        return instance;
    }

    //@Override
    // MSSQL does not support FOR UPDATE, Sybase only supports it beginning with 15.7
    public String generateSelectQuery(final String tableName, final int rowCount) {
        return "SELECT"+(rowCount > 0 ? " TOP "+rowCount : "") + "* FROM " + tableName +
            " WHERE $WHERE";
    }
}
