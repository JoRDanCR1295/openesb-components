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
 * @(#)StreamProjectionAndFilterDerby.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.derby;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.StreamProjectionAndFilter;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.StreamProjectionAndFilterDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.StringUtil;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 */
public class StreamProjectionAndFilterDerby implements StreamProjectionAndFilterDb {
    private static final Messages mMessages = Messages.getMessages(StreamProjectionAndFilterDerby.class);
    
    private DerbySpecial mDerbySpecial;
    
    public StreamProjectionAndFilterDerby(DerbySpecial derbySpecial) {
        mDerbySpecial = derbySpecial;
    }

    public PreparedStatement[] createOperateStatements(Connection con, StreamProjectionAndFilter op) throws Exception {
        String outputQueueName = op.getQueueName();
        Operator inputOp = op.getInputOperatorList().get(0);
        String inputName = inputOp.getName();
        String inputQueueName = inputOp.getQueueName();

        List<Operator> staticInputOperatorList = op.getStaticInputOperatorList();
        List<String> relationInputQueueNameList = new ArrayList<String>();
        List<String> fromColumnList = op.getFromColumnList();
        List<String> toColumnList = op.getToColumnList();

        String[] fromColumnName = new String[fromColumnList.size()];
        Map<String, String> tableNameMap = new HashMap<String, String>();
        tableNameMap.put(inputName, inputQueueName);
        for (Operator o : staticInputOperatorList) {
            String staticInputName = o.getName();
            String staticInputQueueName = o.getQueueName();
            if (o.getOutputType().equals(IO_TYPE_RELATION)) {
                relationInputQueueNameList.add(staticInputQueueName);
            } 
            tableNameMap.put(staticInputName, staticInputQueueName);
        }
        for (int i = 0; i < fromColumnName.length; i++) {
            String exp = fromColumnList.get(i);
            if (exp.startsWith("'") && exp.endsWith("'")) {
                fromColumnName[i] = exp;
            } else {
                fromColumnName[i] = StringUtil.replacePrefixNotStartWithDot(exp, tableNameMap);
            }
        }

        String[] toColumnName = new String[toColumnList.size()];
        for (int i = 0; i < toColumnName.length; i++) {
            toColumnName[i] = toColumnList.get(i);
        }

        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputQueueName);
        sb.append(" (");
        for (int i = 0; i < toColumnName.length; i++) {
            sb.append(toColumnName[i] + ",");
        }
        sb.append(COL_TIMESTAMP + ") ");
        sb.append("(SELECT ");
        for (int i = 0; i < fromColumnName.length; i++) {
            sb.append(fromColumnName[i] + " as " + toColumnName[i] + ",");
        }
        sb.append(inputQueueName + "." + COL_TIMESTAMP + " FROM ");
        String fromClause = StringUtil.replacePrefixNotStartWithDot(op.getFromClause(), tableNameMap);
        sb.append(fromClause);
        sb.append(" WHERE ? < " + inputQueueName + "." + COL_TIMESTAMP + " AND ");
        sb.append(inputQueueName + "." + COL_TIMESTAMP + " <= ?");
        List<String> relationInputQueueNameInFromClauseList = new ArrayList<String>();
        List<String> queueNameInFromClauseList = StringUtil.getTokenList(fromClause, ",");
        for (int i = 0; i < queueNameInFromClauseList.size(); i++) {
            String queueName = queueNameInFromClauseList.get(i).trim();
            if (relationInputQueueNameList.contains(queueName)) {
                relationInputQueueNameInFromClauseList.add(queueName);
            }
        }
        for (int i = 0; i < relationInputQueueNameInFromClauseList.size(); i++) {
            String queueName = relationInputQueueNameInFromClauseList.get(i);
            sb.append(" AND " + queueName + "." + COL_TIMESTAMP + " <= " + inputQueueName + "." + COL_TIMESTAMP);
            sb.append(" AND " + queueName + "." + COL_TAG + " = '+'");
            sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + queueName + " r WHERE ");
            sb.append(" r." + COL_TIMESTAMP + " <= " + inputQueueName + "." + COL_TIMESTAMP);
            sb.append(" AND r." + COL_SEQID + " = " + queueName + "." + COL_SEQID);
            sb.append(" AND r." + COL_TAG + " = '-')");
        }
        String temp = op.getWhereClause();
        if (temp != null && !temp.trim().equals("")) {
            String whereClause = StringUtil.replacePrefixNotStartWithDot(op.getWhereClause(), tableNameMap);
            op.expandWhereClause(whereClause, inputQueueName, relationInputQueueNameList);
            sb.append(" AND ("+ whereClause+ ")");
        }    
        sb.append(")");
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt", new Object[]{op.getName(), sqlStr});
        }    

        return new PreparedStatement[]{con.prepareStatement(sqlStr)};
    }
    
}
