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
 * @(#)TupleBasedWindow.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.Map;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import java.util.HashMap;
import java.util.logging.Level;

/**
 * TupleBasedWindow.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class TupleBasedWindow extends AbstractOperator {
    private static final Messages mMessages = Messages.getMessages(TupleBasedWindow.class);

    protected int mSize;
    
    public TupleBasedWindow(Map prop) {
        initialize(prop);
        mSize = PropertyUtil.getint(prop, PROP_SIZE, 0);
    }

    public String getOutputType() {
        return IO_TYPE_RELATION;
    }

    protected void createOperateStmt(Connection con) throws Exception {
        if (con ==  null) {
            return;
        }

        String inputTableName = mInputOperatorList.get(0).getQueueName();
        Schema inputSchema = mInputOperatorList.get(0).getOutputSchema();
        String outputTableName = getQueueName();

        // Prepare mOperateStmt:
        mOperateStmt = new PreparedStatement[2];

        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" SELECT ");
        for (int i = 0, I = inputSchema.getColumnCount(); i < I; i++) {
            ColumnMetadata cm = inputSchema.getColumnMetadata(i);
            sb.append(cm.getColumnName());
            sb.append(",");
        }
        sb.append(COL_SEQID + "," + COL_TIMESTAMP + ",'+' FROM ");
        sb.append(inputTableName + " u1 ");
        sb.append("WHERE ? < u1." + COL_TIMESTAMP + " AND u1." + COL_TIMESTAMP + " <= ? AND ");
        sb.append("NOT EXISTS (SELECT 'x' FROM " + inputTableName + " u2 ");
        sb.append("WHERE u1." + COL_TIMESTAMP + " = u2." + COL_TIMESTAMP);
        sb.append(" AND u1." + COL_SEQID + " = (u2." + COL_SEQID + " - " + mSize + "))");
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{getName(), "0", sqlStr});
        }    
        mOperateStmt[0] = con.prepareStatement(sqlStr);
        
        sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" SELECT ");
        for (int i = 0, I = inputSchema.getColumnCount(); i < I; i++) {
            ColumnMetadata cm = inputSchema.getColumnMetadata(i);
            sb.append("t.");
            sb.append(cm.getColumnName());
            sb.append(",");
        }
        sb.append("t." + COL_SEQID + ",u." + COL_TIMESTAMP + ",'-' FROM ");
        sb.append(outputTableName);
        sb.append(" t, ");
        sb.append(inputTableName);
        sb.append(" u ");
        sb.append(" WHERE t." + COL_TAG + " = '+' AND ");
        sb.append("? < u." + COL_TIMESTAMP + " AND u." + COL_TIMESTAMP + " <= ? AND ");
        sb.append("t." + COL_SEQID + " = (u." + COL_SEQID + " - " + mSize + ")");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{getName(), "1", sqlStr});
        }    
        mOperateStmt[1] = con.prepareStatement(sqlStr);
    }

    public Map<String, Object> getAdministrableProperties() {
        HashMap<String,Object> map = (HashMap<String,Object>)super.getAdministrableProperties();
        map.put(PROP_SIZE, new Integer(mSize));
        return map;
    }

    public void setAdministrableProperty(String propName, Object propValue) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

}
