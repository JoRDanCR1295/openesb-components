
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
 * @(#)Token.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.util;

import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;
import java.sql.Statement;
import java.sql.Connection;
import java.sql.ResultSet;

/*
 * Token.java
 * 
 * Created on Sep 12, 2007, 3:08:40 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 *
 * @author Bing Lu
 */
import java.util.logging.Level;
public class Token implements OperatorConstants {
    private static final Messages mMessages = Messages.getMessages(Token.class);
    
//	Fortent (mo/jtaylor) 2008-11-21 DB2 fix to resolve bug JR27613
//	private static final String QUERY_ACQUIRE_TOKEN = "SELECT " + COL_ID + "," + COL_NAME + " FROM " + TABLE_EMS_TOKEN + " FOR UPDATE OF " + COL_NAME;
    private static final String QUERY_ACQUIRE_TOKEN = "SELECT " + COL_ID + "," + COL_NAME + " FROM " + TABLE_EMS_TOKEN + " FOR UPDATE";
    private ResultSet mResultSet;
    private Statement mStatement;
    
    public Token(Connection con) throws Exception {
        con.setTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED);
        mStatement = con.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_UPDATABLE, ResultSet.CLOSE_CURSORS_AT_COMMIT);
        mResultSet = mStatement.executeQuery(QUERY_ACQUIRE_TOKEN);
        mResultSet.next();
        mResultSet.updateString(2, "token");
        mResultSet.updateRow();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Token.Token_Acquired");
        }
    }
    
    public void close() {
        Util.close(mResultSet);
        Util.close(mStatement);
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Token.Token_Released");
        }    
    }

}
