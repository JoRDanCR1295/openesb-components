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
 * @(#)TimeBasedWindowDerby.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.derby;

import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.TimeBasedWindow;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.TimeBasedWindowDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class TimeBasedWindowDerby implements TimeBasedWindowDb {
    private static final Messages mMessages = Messages.getMessages(TimeBasedWindowDerby.class);
    
    private DerbySpecial mDerbySpecial;
    
    public TimeBasedWindowDerby(DerbySpecial derbySpecial) {
        mDerbySpecial = derbySpecial;
    }

    public PreparedStatement[] createOperateStatements(Connection con, TimeBasedWindow op) throws Exception {
        String outputTableName = op.getQueueName();
        String inputTableName = op.getInputOperatorList().get(0).getQueueName();
        Schema inputSchema = op.getInputOperatorList().get(0).getOutputSchema();
        double size = op.getSize();
        String unit = op.getUnit();
        long seconds = PropertyUtil.getSeconds(size, unit);
        long miliSec = PropertyUtil.getMiliseconds(size, unit);
        long biliSec = (miliSec - seconds * 1000) * 1000000;

        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" SELECT ");
        String[] inputColumnName = inputSchema.getColumnNames();
        for (int i = 0; i < inputColumnName.length; i++) {
            sb.append(inputColumnName[i] + ",");
        }
        sb.append(COL_SEQID + "," + COL_TIMESTAMP + ",'+' FROM ");
        sb.append(inputTableName);
        sb.append(" WHERE ? < " + COL_TIMESTAMP + " AND " + COL_TIMESTAMP + " <= ?");
        sb.append(" UNION ");
        sb.append("SELECT ");
        for (int i = 0; i < inputColumnName.length; i++) {
            sb.append(inputColumnName[i] + ",");
        }
        sb.append(COL_SEQID + ",{fn TIMESTAMPADD(SQL_TSI_SECOND," + seconds + ", {fn TIMESTAMPADD(SQL_TSI_FRAC_SECOND," + biliSec + "," + COL_TIMESTAMP + ")})},'-' FROM ");
        sb.append(inputTableName);
        sb.append(" WHERE ? < " + COL_TIMESTAMP + " AND " + COL_TIMESTAMP + " <= ?");
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt", new Object[]{op.getName(), sqlStr});
        }    

        return new PreparedStatement[]{con.prepareStatement(sqlStr)};
    }
}
