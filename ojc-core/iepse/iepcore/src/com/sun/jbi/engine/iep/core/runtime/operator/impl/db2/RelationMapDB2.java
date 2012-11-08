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
 * @(#)RelationMapOracle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl.db2;

import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.RelationMapDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.RelationMap;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.StringUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

/**
 *
 * @author Bing Lu
 * 
 * Fortent : Based on RelationMapOracle.java
 */
public class RelationMapDB2 implements RelationMapDb {
    private static final Messages mMessages = Messages.getMessages(RelationMapDB2.class);
    
    private DB2Special mDB2Special;
    
    public RelationMapDB2(DB2Special dB2Special) {
    	mDB2Special = dB2Special;
    }

    public void createStoredProcedures(Connection con) {
    }

    public void dropStoredProcedures(Connection con) {
    }

    public PreparedStatement[] createOperateStatements(Connection con, RelationMap op) throws Exception {
        // Prepare ret:
        PreparedStatement[] ret = new PreparedStatement[2];
        
        QueryPlan plan = op.getPlan();
        String outputTableName = op.getQueueName();
        List<Operator> inputOperatorList = op.getInputOperatorList();
        int inputCount = inputOperatorList.size();
        List<Operator> staticInputOperatorList = op.getStaticInputOperatorList();
        List<String> fromColumnList = op.getFromColumnList();
        List<String> toColumnList = op.getToColumnList();
        
        String inputTableName[] = new String[inputCount];
        String seqIdColumnName[] = new String[inputCount];
        Map<String, String> tableNameMap = new HashMap<String, String>();
        for (int i = 0; i < inputCount; i++) {
            Operator o = inputOperatorList.get(i);
            String inputName = o.getName();
            inputTableName[i] = o.getQueueName();
            seqIdColumnName[i] = op.getSeqIdColumnName(inputTableName[i]);
            tableNameMap.put(inputName, inputTableName[i]);
        }
        int staticTableCount = staticInputOperatorList.size();
        for (int i = 0; i < staticTableCount; i++) {
            Operator o = staticInputOperatorList.get(i);
            String staticInputName = o.getName();
            String staticInputTableName = o.getQueueName();
            tableNameMap.put(staticInputName, staticInputTableName);
        }
        
        String[] fromColumnName = new String[fromColumnList.size()];
        for (int i = 0; i < fromColumnName.length; i++) {
            String exp = fromColumnList.get(i);
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
            toColumnName[i] = toColumnList.get(i);
        }
        
        String fromClause = StringUtil.replacePrefixNotStartWithDot(op.getFromClause(), tableNameMap);
        
        String whereClause = StringUtil.replacePrefixNotStartWithDot(op.getWhereClause(), tableNameMap);
        
        String sequenceName = Util.getSequenceName(plan.getId(), op.getId());
        
        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO " + outputTableName + " (");
        for (int i = 0; i < toColumnName.length; i++) {
            sb.append(toColumnName[i] + ",");
        }
        for (int i = 0; i < inputCount; i++) {
            sb.append(seqIdColumnName[i] + ",");
        }
        sb.append(COL_SEQID + "," + COL_TIMESTAMP + "," + COL_TAG + ") SELECT ");
        for (int i = 0; i < toColumnName.length; i++) {
            sb.append(toColumnName[i] + ",");
        }
        for (int i = 0; i < inputCount; i++) {
            sb.append(seqIdColumnName[i] + ",");
        }

//      sb.append(sequenceName);
//      sb.append(".NextVal,");
//      sb.append(COL_TIMESTAMP + "," + COL_TAG + " FROM (");
//		Fortent : sequence change
        sb.append("NEXT VALUE FOR " + sequenceName + "," + COL_TIMESTAMP + "," + COL_TAG + " FROM (");

        for (int i = 0; i < inputCount; i++) {
            if (i > 0) {
                sb.append(" UNION ALL ");
            }
            sb.append("SELECT ");
            for (int j = 0; j < fromColumnName.length; j++) {
                sb.append(fromColumnName[j] + " AS " + toColumnName[j] + ",");
            }
            for (int j = 0; j < inputCount; j++) {
                sb.append(inputTableName[j] + "." + COL_SEQID + " AS " + seqIdColumnName[j] + ",");
            }
            sb.append(inputTableName[i] + "." + COL_TIMESTAMP + " AS " + COL_TIMESTAMP + ",'+' AS " + COL_TAG);
            sb.append(" FROM " + fromClause + " WHERE ");
            for (int j = 0; j < inputCount; j++) {
                sb.append(inputTableName[j] + "." + COL_TAG + " = '+' AND ");
                if (j < i) {
                    sb.append(inputTableName[j] + "." + COL_TIMESTAMP + " < " + inputTableName[i] + "." + COL_TIMESTAMP + " AND ");
                } else if (j == i) {
                    sb.append("? < " + inputTableName[j] + "." + COL_TIMESTAMP + " AND ");
                    sb.append(inputTableName[j] + "." + COL_TIMESTAMP + " <= ? AND ");
                } else {// j > i
                    sb.append(inputTableName[j] + "." + COL_TIMESTAMP + " <= " + inputTableName[i] + "." + COL_TIMESTAMP + " AND ");
                }
                if (j != i) {
                    sb.append("NOT EXISTS (SELECT 'x' FROM " + inputTableName[j] + " r WHERE ");
                    sb.append("r." + COL_SEQID + " = " + inputTableName[j] + "." + COL_SEQID + " AND ");
                    sb.append("r." + COL_TAG + " = '-' AND r." + COL_TIMESTAMP);
                    sb.append(" <= ");
                    sb.append(inputTableName[i] + "." + COL_TIMESTAMP + ") AND ");
                }
            }
            if (whereClause.trim().equals("")) {
                // Delete "AND " at end
                int len = sb.length();
                sb.delete(len-4, len);
            } else {
                sb.append("(" + whereClause + ")");
            }
        }
        sb.append(" ORDER BY " + COL_TIMESTAMP + ")");

//		Fortent : correlation name change
        sb.append( " x" );
        
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "0", sqlStr});
        }    
        ret[0] = con.prepareStatement(sqlStr);
        
        String t[] = new String[inputCount];
        for (int i = 0; i < inputCount; i++) {
            t[i] = "t" + (i + 1);
        }
        sb = new StringBuffer();
        sb.append("INSERT INTO " + outputTableName + " (");
        for (int i = 0; i < toColumnName.length; i++) {
            sb.append(toColumnName[i] + ",");
        }
        for (int i = 0; i < inputCount; i++) {
            sb.append(seqIdColumnName[i] + ",");
        }
        sb.append(COL_SEQID + "," + COL_TIMESTAMP + "," + COL_TAG + ") SELECT ");
        for (int i = 0; i < toColumnName.length; i++) {
            sb.append("u." + toColumnName[i] + ",");
        }
        for (int i = 0; i < inputCount; i++) {
            sb.append("u." + seqIdColumnName[i] + ",");
        }
        sb.append("u." + COL_SEQID + ",v." + COL_TIMESTAMP + ",'-' FROM ");
        sb.append(outputTableName + " u, ");
        sb.append("(SELECT DISTINCT ");
        for (int i = 0; i < inputCount; i++) {
            sb.append(seqIdColumnName[i] + ",");
        }
        sb.append("MIN(" + COL_TIMESTAMP + ") AS " + COL_TIMESTAMP + " FROM (");
        for (int i = 0; i < inputCount; i++) {
            if (i > 0) {
                sb.append(" UNION ALL ");
            }
            sb.append(" SELECT ");
            for (int j = 0; j < inputCount; j++) {
                sb.append("r." + seqIdColumnName[j] + " AS " + seqIdColumnName[j] + ",");
            }
            sb.append(t[i] + "." + COL_TIMESTAMP + " AS " + COL_TIMESTAMP);
            sb.append(" FROM " + inputTableName[i] + " " + t[i] + ", " + outputTableName + " r WHERE ");
            sb.append(t[i]+ "." + COL_SEQID + " = r."+ seqIdColumnName[i] + " AND ");
            sb.append(t[i] + "." + COL_TAG+ " = '-' AND ");
            sb.append("? < " + t[i] + "." + COL_TIMESTAMP + " AND ");
            sb.append(t[i] + "." + COL_TIMESTAMP + " <= ? ");
        }
        sb.append(") w GROUP BY ");
        for (int i = 0; i < inputCount; i++) {
            if (i > 0) {
                sb.append(", ");
            }
            sb.append(seqIdColumnName[i]);
        }
        sb.append(") v WHERE ");
        for (int i = 0; i < inputCount; i++) {
            if (i > 0) {
                sb.append(" AND ");
            }
            sb.append("u." + seqIdColumnName[i] + " = v." + seqIdColumnName[i]);
        }
        sb.append(" AND NOT EXISTS (SELECT 'x' FROM " + outputTableName + " x WHERE ");
        sb.append("x." + COL_SEQID + " = u." + COL_SEQID + " AND ");
        sb.append("x." + COL_TAG + " = '-')");
        sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt_i", new Object[]{op.getName(), "1", sqlStr});
        }    
        ret[1] = con.prepareStatement(sqlStr);
        
        return ret;
    }
    
    public void executeOperateStatements(RelationMap op, Timestamp prevT, Timestamp curT) throws Exception {
        int inputCount = op.getInputOperatorList().size();
        for (int i = 0; i < inputCount; i++) {
            op.getOperateStatement(0).setTimestamp(2*i+1, prevT);
            op.getOperateStatement(0).setTimestamp(2*i+2, curT);
        }
        op.getOperateStatement(0).executeUpdate();
        for (int i = 0; i < inputCount; i++) {
            op.getOperateStatement(1).setTimestamp(2*i+1, prevT);
            op.getOperateStatement(1).setTimestamp(2*i+2, curT);
        }
        op.getOperateStatement(1).executeUpdate();
    }
}
