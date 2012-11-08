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
 * @(#)OracleSpecial.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator.impl.oracle;

import com.sun.jbi.engine.iep.core.runtime.operator.impl.*;
import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;

/*
 * OracleSpecial.java
 *
 * Created on August 22, 2005, 1:08 PM
 *
 * @author Bing Lu
 */
public class OracleSpecial extends AbstractDbSpecial {
    private static final Messages mMessages = Messages.getMessages(OracleSpecial.class);
   
    public static String MILLI_SECOND_PER_DAY = "(24 * 3600 * 1000)";
    
    private AttributeBasedWindowDb mAttributeBasedWindowDb = new AttributeBasedWindowOracle(this);
    private ContiguousOrderDb mContiguousOrderDb = new ContiguousOrderOracle(this);
    private DeleteStreamDb mDeleteStreamDb = new DeleteStreamOracle(this);
    private DistinctDb mDistinctDb = new DistinctOracle(this);
    private ExternalTablePollingStreamDb mExternalTablePollingStreamDb = new ExternalTablePollingStreamOracle(this);
    private ReplayStreamDb mReplayStreamDb = new ReplayStreamOracle(this);
    private GapWindowDb mGapWindowDb = new GapWindowOracle(this);
    private InsertStreamDb mInsertStreamDb = new InsertStreamOracle(this);
    private IntersectDb mIntersectDb = new IntersectOracle(this);
    private InvokeServiceDb mInvokeServiceDb = new InvokeServiceOracle(this);
    private InvokeStreamDb mInvokeStreamDb = new InvokeStreamOracle(this);
    private MergeDb mMergeDb = new MergeOracle(this);
    private MinusDb mMinusDb = new MinusOracle(this);
    private NotificationStreamDb mNotificationStreamDb = new NotificationStreamOracle(this);
    private PartitionedWindowDb mPartitionedWindowDb = new PartitionedWindowOracle(this);
    private RelationAggregatorDb mRelationAggregatorDb = new RelationAggregatorOracle(this);
    private RelationMapDb mRelationMapDb = new RelationMapOracle(this);
    private RelationStreamDb mRelationStreamDb = new RelationStreamOracle(this);
    private StreamInputDb mStreamInputDb = new StreamInputOracle(this);
    private StreamProjectionAndFilterDb mStreamProjectionAndFilterDb = new StreamProjectionAndFilterOracle(this);
    private TimeBasedAggregatorDb mTimeBasedAggregatorDb = new TimeBasedAggregatorOracle(this);
    private TimeBasedWindowDb mTimeBasedWindowDb = new TimeBasedWindowOracle(this);
    private TupleBasedAggregatorDb mTupleBasedAggregatorDb = new TupleBasedAggregatorOracle(this);
    private TupleSerialCorrelationDb mTupleSerialCorrelationDb = new TupleSerialCorrelationOracle(this);
    private UnionAllDb mUnionAllDb = new UnionAllOracle(this);
    private UnionDb mUnionDb = new UnionOracle(this);
    
    /** Creates a new instance of OracleSpecial */
    public OracleSpecial() {
    }
    
    public void createStoredProcedures(Connection con) throws Exception {
    }
    
    public void dropStoredProcedures(Connection con) throws Exception {
    }
    
    public PreparedStatement createCleanRelationByMinUsageTimeStmt(Connection con, String planId, String tableName) throws Exception {
        // DELETE FROM R1 t1
        // WHERE EXISTS (SELECT 'x' FROM R1 WHERE Tag='-' AND Row_Id = t1.Row_Id AND Timestamp <= min_usage_time)
        StringBuffer sb = new StringBuffer();
        sb.append("DELETE FROM ");
        sb.append(tableName);
        sb.append(" t1 WHERE EXISTS (SELECT 'x' FROM ");
        sb.append(tableName);
        sb.append(" WHERE " + COL_TAG + "='-' AND " + COL_SEQID + "= t1." + COL_SEQID);
        sb.append(" AND " + COL_TIMESTAMP + " <= (SELECT MIN(" + COL_TIMESTAMP + ") FROM ");
        sb.append(Util.getTableUsageTableName(planId));
        sb.append(" WHERE " + COL_TABLE_NAME + " = '" );
        sb.append(tableName);
        sb.append("'))");
        String sqlStr = sb.toString();
        return con.prepareStatement(sqlStr);
    }
    
    public String getNativeSqlType(String jdbcType) {
        // No change for following types
        // SQL_TYPE_CHAR
        // SQL_TYPE_INTEGER
        // SQL_TYPE_DATE
        // SQL_TYPE_TIME
        // SQL_TYPE_TIMESTAMP
        
        // Change for following types
        if (SQL_TYPE_BIGINT.equals(jdbcType)) {
            return "NUMBER";
        }
        if (SQL_TYPE_DOUBLE.equals(jdbcType)) {
            return "FLOAT";
        }
        if (SQL_TYPE_VARCHAR.equals(jdbcType)) {
            return "VARCHAR2";
        }
        return jdbcType;
    }

    public void createTable(Connection con, String tableName, Schema schema) throws Exception {
        List<ColumnMetadata> tailColumns = new ArrayList<ColumnMetadata>();
        createTableWithTailColumnsAndConstraints(con, tableName, schema, tailColumns, Collections.EMPTY_LIST);
    }

    public void createStream(Connection con, String tableName, Schema schema, List<ColumnMetadata> extraColumns, boolean autoGenIfPossible) throws Exception {
        extraColumns.add(new ColumnMetadata(COL_SEQID, SQL_TYPE_NUMERIC));
        extraColumns.add(new ColumnMetadata(COL_TIMESTAMP, SQL_TYPE_TIMESTAMP));
        createTableWithTailColumnsAndConstraints(con, tableName, schema, extraColumns, Collections.EMPTY_LIST);
    }

    public void createRelation(Connection con, String tableName, Schema schema, List<ColumnMetadata> extraColumns, boolean autoGenIfPossible) throws Exception {
        extraColumns.add(new ColumnMetadata(COL_SEQID, SQL_TYPE_NUMERIC));
        extraColumns.add(new ColumnMetadata(COL_TIMESTAMP, SQL_TYPE_TIMESTAMP));
        extraColumns.add(new ColumnMetadata(COL_TAG, SQL_TYPE_CHAR, 1));
        createTableWithTailColumnsAndConstraints(con, tableName, schema, extraColumns, Collections.EMPTY_LIST);
    }

    public void createTableWithTailColumnsAndConstraints(Connection con, String tableName, Schema schema, List<ColumnMetadata> tailColumns, List<String> constraints) throws Exception {
        Statement stmt = null;
        StringBuffer sql = new StringBuffer();
        try {
            sql.append("CREATE TABLE ");
            sql.append(tableName);
            sql.append(" (");
            for (int i = 0, I = schema.getColumnCount(); i < I; i++) { 
                ColumnMetadata columnMetadata = schema.getColumnMetadata(i);
                String columnType = columnMetadata.getColumnType();
                // name type[(size[,scale])], ..
                sql.append(columnMetadata.getColumnName());
                sql.append(" ");
                sql.append(getNativeSqlType(columnType));
                if (SQL_TYPE_BIGINT.equals(columnType)) {
                    // NUMBER[(size)]
                    if (columnMetadata.hasColumnSize()) {
                        sql.append("(");
                        sql.append("" + columnMetadata.getColumnSize());
                        sql.append(")");
                    } 
                } else if (SQL_TYPE_VARCHAR.equals(columnType)) {
                    //VARCHAR2[(size default 100)]
                    sql.append("(");
                    int size = columnMetadata.hasColumnSize()? columnMetadata.getColumnSize() : 100;
                    sql.append("" + size);
                    sql.append(")");
                }
                sql.append(",");
            }
            for (ColumnMetadata columnMetadata : tailColumns) { 
                // name type[(size[,scale])], ..
                String columnType = columnMetadata.getColumnType();
                sql.append(columnMetadata.getColumnName());
                sql.append(" ");
                sql.append(getNativeSqlType(columnMetadata.getColumnType()));
                if (SQL_TYPE_BIGINT.equals(columnType)) {
                    // NUMBER[(size)]
                    if (columnMetadata.hasColumnSize()) {
                        sql.append("(");
                        sql.append("" + columnMetadata.getColumnSize());
                        sql.append(")");
                    } 
                } else if (SQL_TYPE_VARCHAR.equals(columnType)) {
                    //VARCHAR2[(size default 100)]
                    sql.append("(");
                    int size = columnMetadata.hasColumnSize()? columnMetadata.getColumnSize() : 100;
                    sql.append("" + size);
                    sql.append(")");
                }
                sql.append(",");
            }
            for (String c : constraints) {
                sql.append(c + ",");
            }
            sql.setCharAt(sql.length()-1, ')'); // replace the last ", "
            stmt = con.createStatement();
            //stmt.executeQuery(sql.toString());
            stmt.execute(sql.toString());
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "OracleSpecial.Create_table_with", sql.toString());
            }
        } catch (Exception e) {
            throw new Exception(mMessages.getString("OracleSpecial.Fail_to_create_table_with", sql.toString()), e);
        } finally {
            Util.close(stmt);
        }
    }
    
    public boolean isSequenceExists(Connection con, String sequenceName) {
        //SELECT sequence_name, last_number FROM user_sequences;
        boolean exists = false;
        Statement stmt = null;
        try {
            stmt = con.createStatement();
            ResultSet rs = stmt.executeQuery("SELECT sequence_name FROM user_sequences");
            while(rs.next()) {
                String seqName = rs.getString(1);
                if(sequenceName.equals(seqName)) {
                    exists = true;
                    break;
                }
            }
        } catch (Exception e) {
            //log exception
            mMessages.log(Level.SEVERE, "OracleSpecial.Fail_to_find_sequence", sequenceName, e);
            
        } finally {
            Util.close(stmt);
        }
        return exists;
    }
    
    public void createSequence(Connection con, String sequenceName) throws Exception {
        // CREATE SEQUENCE sequenceName INCREMENT BY 1 START WITH 1;
        Statement stmt = null;
        try {
            stmt = con.createStatement();
            stmt.execute("CREATE SEQUENCE " + sequenceName + " INCREMENT BY 1 START WITH 1 NOCACHE ORDER");
        } catch (Exception e) {
            throw e;
        } finally {
            Util.close(stmt);
        }
    }

    public void createSequence(Connection con, Operator op) throws Exception {
        // CREATE SEQUENCE S1_seqid INCREMENT BY 1 START WITH 1;
        String planId = op.getPlan().getId();
        String opId = op.getId();
        String seqName = Util.getSequenceName(planId, opId);
        try {
            createSequence(con, seqName);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "OracleSpecial.Fail_to_create_sequence", seqName, e);
        } 
    }
    
    public void dropSequence(Connection con, Operator op) throws Exception {
        // DROP SEQUENCE S1_seqid
        String planId = op.getPlan().getId();
        String opId = op.getId();
        String seqName = Util.getSequenceName(planId, opId);
        Statement stmt = null;
        try {
            stmt = con.createStatement();
            stmt.execute("DROP SEQUENCE " + seqName);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "OracleSpecial.Fail_to_drop_sequence", seqName, e);
        } finally {
            Util.close(stmt);
        }
    }
    
    public void createIndex(Connection con, String indexName, String tableName, List<String> columnNameList) throws Exception {
        Statement stmt = null;
        try {
            StringBuffer sb = new StringBuffer();
            sb.append("CREATE INDEX " + indexName + " ON " + tableName + "(");
            for (int i = 0; i < columnNameList.size(); i++) {
                if (i > 0) {
                    sb.append(",");
                }
                sb.append(columnNameList.get(i));
            }
            sb.append(")");
            stmt = con.createStatement();
            stmt.execute(sb.toString());
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Fail_to_create_index_on_table", new Object[]{indexName, tableName}, e);
        } finally {
            Util.close(stmt);
        }

    }
    public void dropIndex(Connection con, String indexName, String tableName) throws Exception {
        Statement stmt = null;
        try {
            stmt = con.createStatement();
            stmt.execute("DROP INDEX " + indexName);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Fail_to_drop_index", indexName, e);
        } finally {
            Util.close(stmt);
        }
    }

    public PreparedStatement getRelationResultForDebugInfo(Connection con, String inputTableName, String[] cols) throws Exception {
        PreparedStatement s = null;

        StringBuffer sb = new StringBuffer();
        sb.append("SELECT DISTINCT ");
        for (String col : cols) {
            sb.append("t1." + col + ",");
        }
        sb.append("t2." + COL_TIMESTAMP + " FROM ");
        sb.append(inputTableName);
        sb.append(" t1,");
        sb.append(inputTableName);
        sb.append(" t2 WHERE t1." + COL_TAG + " = '+' AND t1." + COL_TIMESTAMP + " <= t2." + COL_TIMESTAMP);
        sb.append(" AND ? < t2." + COL_TIMESTAMP + " AND t2." + COL_TIMESTAMP + " <= ? AND ");
        sb.append("NOT EXISTS (SELECT 'x' FROM ");
        sb.append(inputTableName + " t3");
        sb.append(" WHERE " + "t3." + COL_SEQID + " = t1." + COL_SEQID + " AND " + "t3." + COL_TIMESTAMP + " <= t2." + COL_TIMESTAMP + " AND " + "t3." + COL_TAG + " = '-')");
        sb.append(" ORDER BY t2." + COL_TIMESTAMP + ", t1." + COL_SEQID);
        String sqlStr = sb.toString();
        s = con.prepareStatement(sqlStr);
        return s;
    }

    public void dropTable(String tableName, Connection con) throws Exception {
        StringBuffer sql = new StringBuffer();
        sql.append("DROP TABLE ");
        sql.append(tableName);
        
        PreparedStatement ps = con.prepareStatement(sql.toString());
        
        ps.executeUpdate();
    }
    
    public AttributeBasedWindowDb getAttributeBasedWindowDb() {
        return mAttributeBasedWindowDb;
    } 

    public ContiguousOrderDb getContiguousOrderDb() {
        return mContiguousOrderDb;
    }
    
    public DeleteStreamDb getDeleteStreamDb() {
        return mDeleteStreamDb;
    }
    
    public DistinctDb getDistinctDb() {
        return mDistinctDb;
    }
    
    public ExternalTablePollingStreamDb getExternalTablePollingStreamDb() {
        return mExternalTablePollingStreamDb;
    }

    public ReplayStreamDb getReplayStreamDb() {
        return mReplayStreamDb;
    }
    
    public GapWindowDb getGapWindowDb() {
        return mGapWindowDb;
    }

    public InsertStreamDb getInsertStreamDb() {
        return mInsertStreamDb;
    }
    
    public IntersectDb getIntersectDb() {
        return mIntersectDb;
    }
    
    public InvokeServiceDb getInvokeServiceDb() {
        return mInvokeServiceDb;
    }
    
    public InvokeStreamDb getInvokeStreamDb() {
        return mInvokeStreamDb;
    }
    
    public MergeDb getMergeDb() {
        return mMergeDb;
    }

    public MinusDb getMinusDb() {
        return mMinusDb;
    }
    
    public NotificationStreamDb getNotificationStreamDb() {
        return mNotificationStreamDb;
    }
    
    public PartitionedWindowDb getPartitionedWindowDb() {
        return mPartitionedWindowDb;
    }
    
    public RelationAggregatorDb getRelationAggregatorDb() {
        return mRelationAggregatorDb;
    }
    
    public RelationMapDb getRelationMapDb() {
        return mRelationMapDb;
    }
    
    public RelationStreamDb getRelationStreamDb() {
        return mRelationStreamDb;
    }
    
    public StreamInputDb getStreamInputDb() {
        return mStreamInputDb;
    }
    
    public StreamProjectionAndFilterDb getStreamProjectionAndFilterDb() {
        return mStreamProjectionAndFilterDb;
    }
    
    public TimeBasedAggregatorDb getTimeBasedAggregatorDb() {
        return mTimeBasedAggregatorDb;
    }
    
    public TimeBasedWindowDb getTimeBasedWindowDb() {
        return mTimeBasedWindowDb;
    }
    
    public TupleBasedAggregatorDb getTupleBasedAggregatorDb() {
        return mTupleBasedAggregatorDb;
    }
    
    public TupleSerialCorrelationDb getTupleSerialCorrelationDb() {
        return mTupleSerialCorrelationDb;
    }
    public UnionAllDb getUnionAllDb() {
        return mUnionAllDb;
    }
    
    public UnionDb getUnionDb() {
        return mUnionDb;
    }
    

}
