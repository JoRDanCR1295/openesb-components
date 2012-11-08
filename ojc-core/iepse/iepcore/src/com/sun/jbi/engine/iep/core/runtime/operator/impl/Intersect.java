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
 * @(#)Intersect.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.util.Map;
import java.util.ArrayList;
import java.sql.Timestamp;
import java.sql.Connection;
import java.sql.Statement;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.util.logging.Level;

/**
 * Intersect.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class Intersect extends AbstractOperator {
    private static final Messages mMessages = Messages.getMessages(Intersect.class);
    
    
    public Intersect(Map prop) {
        initialize(prop);
    }
    
    public String getOutputType() {
        return IO_TYPE_RELATION;
    }

    public String getViewName() {
        return "v_" + mQueryPlan.getId() + "_" +  mId;
    }
        
    private void createView(Connection con) throws Exception {
        // CREATE VIEW v_planId_opId (Name, Value, Timetamp, Tag) AS
        //    SELECT Name, Value, Timestamp, Tag FROM R1
        //    UNION
        //    SELECT Name, Value, Timestamp, Tag FROM R2
        // Notice:
        //   "UNION" so that no duplicate '+' event with same (Name, Value, Timestamp) will trigger 
        //      adding '+' in R twice, and no duplicate '-' event with same (Name, Value, Timestamp) 
        //      will trigger adding '-' in R twice.
        String name = getViewName();
        Statement stmt = null;
        try {
            Schema schema = getOutputSchema();
            String[] columnNames = schema.getColumnNames();
            int inputCount = mInputOperatorList.size();
            String inputTableName[] = new String[inputCount];
            for (int i = 0; i < inputCount; i++) {
                inputTableName[i] = mInputOperatorList.get(i).getQueueName();
            }
            
            StringBuffer sb = new StringBuffer();
            sb.append("CREATE VIEW " + name + " (");
            for (int i = 0; i < columnNames.length; i++) {
                sb.append(columnNames[i] + ", ");
            }
            sb.append(COL_TIMESTAMP + ", " + COL_TAG + ") AS ");
            for (int i = 0; i < inputCount; i++) {
                if (0 < i) {
                    sb.append(" UNION ");
                }
                sb.append("SELECT ");
                for (int j = 0; j < columnNames.length; j++) {
                    sb.append(columnNames[j] + ", ");
                }
                sb.append(COL_TIMESTAMP + ", " + COL_TAG + " FROM " + inputTableName[i]);
            }
            String sqlStr = sb.toString();
            stmt = con.createStatement();
            stmt.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Fail_to_create_view", name, e);
        } finally {
            Util.close(stmt);
        }
    }
    
    private void dropView(Connection con) throws Exception {
        // DROP VIEW v_planId_opId
        String name = getViewName();
        Statement stmt = null;
        try {
            stmt = con.createStatement();
            stmt.execute("DROP VIEW " + name);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Fail_to_drop_view", name, e);
        } finally {
            Util.close(stmt);
        }
    }
    // template method: used by deploy()
    @Override
    protected void createOutputQueue(Connection con) throws Exception {
        Schema schema = getOutputSchema();
        String tableName = getQueueName();
        mDbSpecial.createRelation(con, tableName, schema, new ArrayList<ColumnMetadata>(), true);
        createView(con);
    }

    // template method: used by undeploy()
    @Override
    protected void dropOutputQueue(Connection con) throws Exception {
        String tableName = getQueueName();
        // drop index before drop the table on which it is defined
        dropView(con);
        Util.dropTable(con, tableName);
    }
    
    @Override
    protected void createSynopsis(Connection con) throws Exception {
        mDbSpecial.createSequence(con, this);
    }
    
    @Override
    protected void dropSynopsis(Connection con) throws Exception {
        mDbSpecial.dropSequence(con, this);
    }
    
    @Override
    protected void createOperateStmt(Connection con) throws Exception {
        if (con ==  null) {
            return;
        }
        // Prepare mOperateStmt:
        mOperateStmt = mDbSpecial.getIntersectDb().createOperateStatements(con, this);
    }

    @Override
    protected void executeOperateStmt(Timestamp prevT, Timestamp curT) throws Exception {
        mOperateStmt[0].setTimestamp(1, prevT);
        mOperateStmt[0].setTimestamp(2, curT);
        mOperateStmt[0].executeUpdate();
        
        mOperateStmt[1].setTimestamp(1, prevT);
        mOperateStmt[1].setTimestamp(2, curT);
        mOperateStmt[1].executeUpdate();
    }

}

        
