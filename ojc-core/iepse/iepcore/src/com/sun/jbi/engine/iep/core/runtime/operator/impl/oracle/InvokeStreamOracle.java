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
 * @(#)InvokeStreamOracle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.oracle;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.InvokeStream;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.InvokeStreamDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.StringUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class InvokeStreamOracle implements InvokeStreamDb {
    private static final Messages mMessages = Messages.getMessages(InvokeStreamOracle.class);
    
    private OracleSpecial mOracleSpecial;
    
    public InvokeStreamOracle(OracleSpecial oracleSpecial) {
        mOracleSpecial = oracleSpecial;
    }

    public PreparedStatement[] createOperateStatements(Connection con, InvokeStream op) throws Exception {
//        do insert into external table, select from source select view
        QueryPlan plan = op.getPlan();
        String seqName = Util.getSequenceName(plan.getId(), op.getId());
        String outputTableName = op.findExternalOperatorTableName(con);
        Operator inputOp = op.getInputOperatorList().get(0);
        String inputName = inputOp.getName();
        String inputTableName = inputOp.getQueueName();
        
        List staticInputIdList = op.getStaticInputIdList();
        List fromColumnList = op.getFromColumnList();
        List toColumnList = op.getToColumnList();
        
        String[] fromColumnName = new String[fromColumnList.size()];
        Map<String, String> tableNameMap = new HashMap<String, String>();
        tableNameMap.put(inputName, inputTableName);
        for (int i = 0; i < staticInputIdList.size(); i++) {
            String staticInputId = (String)staticInputIdList.get(i);
            Operator o = plan.getOperatorById(staticInputId);
            String staticInputName = o.getName();
            String staticInputTableName = o.getQueueName();
            tableNameMap.put(staticInputName, staticInputTableName);
        }
        for (int i = 0; i < fromColumnName.length; i++) {
            String exp = (String)fromColumnList.get(i);
            if (exp.startsWith("'") && exp.endsWith("'")) {
                // exp is an expression whose value may be some valid fromColcolumn
                // hence we cannot process it with StringUtil.replacePrefixNotStartWithDot.
                fromColumnName[i] = exp;
            } else {
                fromColumnName[i] = StringUtil.replacePrefixNotStartWithDot(exp, tableNameMap);
            }
            
        }
        
        String[] toColumnName = new String[toColumnList.size()];
        for (int i = 0; i < toColumnName.length; i++) {
            toColumnName[i] = (String)toColumnList.get(i);
        }
        // Prepare mOperateStmt:
        // INSERT INTO S2 (toColumn1, toColumn2, SeqId, Timestamp)
        //    SELECT toColumn1, toColumn2, S1_seqid.nextVal, Timestamp FROM (
        //       SELECT fromColumn1 as toColumn1, fromColumn2 as toColumn2, S1.Timestamp FROM fromClause(T1, T2, S1)
        //       WHERE 7 < S1.Timestamp AND S1.Timestamp <= 10 AND whereClause(T1, T2, S1)
        //       ORDER BY S1.Timestamp);
        
        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" (");
        for (int i = 0; i < toColumnName.length; i++) {
            sb.append(toColumnName[i]);
            sb.append(",");
        }
        sb.append(COL_SEQID + "," + COL_TIMESTAMP + ") SELECT ");
        for (int i = 0; i < toColumnName.length; i++) {
            sb.append(toColumnName[i]);
            sb.append(",");
        }
        sb.append(seqName);
        sb.append(".NextVal,");
        sb.append(COL_TIMESTAMP + " FROM (SELECT ");
        for (int i = 0; i < fromColumnName.length; i++) {
            sb.append(fromColumnName[i]);
            sb.append(" as ");
            sb.append(toColumnName[i]);
            sb.append(",");
        }
        //sb.append(inputTableName + "." + COL_TIMESTAMP + " FROM ");
        sb.append("CURRENT_TIMESTAMP as ");
        sb.append(COL_TIMESTAMP);
        sb.append(" FROM ");
        String fromClause = StringUtil.replacePrefixNotStartWithDot(op.getFromClause(), tableNameMap);
        sb.append(fromClause);
        sb.append(" WHERE ? < ");
        sb.append(inputTableName + "." + COL_TIMESTAMP + " AND ");
        sb.append(inputTableName + "." + COL_TIMESTAMP + " <= ?");
        String temp = op.getWhereClause();
        if (temp != null && !temp.trim().equals("")) {
            sb.append(" AND ");
            String whereClause = StringUtil.replacePrefixNotStartWithDot(op.getWhereClause(), tableNameMap);
            sb.append(whereClause);
        }    
        sb.append(" ORDER BY " + inputTableName + "." + COL_TIMESTAMP + ")");
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt", new Object[]{op.getName(), sqlStr});
        }    
        return new PreparedStatement[]{con.prepareStatement(sqlStr)};
    }
    
}
