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
 * @(#)StreamInputDerby.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.iep.core.runtime.operator.impl.derby;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.StreamInput;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.StreamInputDb;
import java.sql.Connection;
import java.sql.PreparedStatement;

/**
 *
 * @author Bing Lu
 */
public class StreamInputDerby implements StreamInputDb {
    
    private DerbySpecial mDerbySpecial;
    
    public StreamInputDerby(DerbySpecial derbySpecial) {
        mDerbySpecial = derbySpecial;
    }
    
    public PreparedStatement createInsertStatement(Connection con, StreamInput op,boolean includeDBCommandForTS) throws Exception {
        String tableName = op.getQueueName();
        Schema schema = op.getOutputSchema();
        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(tableName);
        sb.append("(");
        for (int i = 0, I = schema.getColumnCount(); i < I; i++) {
            ColumnMetadata cmd = schema.getColumnMetadata(i);
            sb.append(cmd.getColumnName() + ",");
        }
        sb.append(COL_TIMESTAMP);
        sb.append(") VALUES (");
        for (int i = 0, I = schema.getColumnCount(); i < I; i++) {
            sb.append("?,");
        }
        if(includeDBCommandForTS) {
            //CURRENT_TIMESTAMP_NS() relies on System.nanoTime() which is not accurate when used on
            //Solaris, so temporarily using the SQL CURRENT_TIMESTAMP */
            //sb.append("CURRENT_TIMESTAMP_NS()");
            sb.append("CURRENT_TIMESTAMP");
        } else {
            sb.append("?");
        }
        sb.append(")");
        String sqlStr = sb.toString();
        PreparedStatement stmt = con.prepareStatement(sqlStr);
        return stmt;
    }
}
