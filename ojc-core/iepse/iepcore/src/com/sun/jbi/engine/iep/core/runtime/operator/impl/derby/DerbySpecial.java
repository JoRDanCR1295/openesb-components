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
 * @(#)DerbySpecial.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator.impl.derby;

import com.sun.jbi.engine.iep.core.runtime.operator.impl.*;
import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
/*
 * DerbySpecial.java
 *
 * Created on August 22, 2005, 1:08 PM
 *
 * @author Bing Lu
 */
public class DerbySpecial extends AbstractDbSpecial {
    private static final Messages mMessages = Messages.getMessages(DerbySpecial.class);

    private String mIepDerbyJarPath;
    private String mDbSchemaName;
    
    private AttributeBasedWindowDb mAttributeBasedWindowDb = new AttributeBasedWindowDerby(this);
    private ContiguousOrderDb mContiguousOrderDb = new ContiguousOrderDerby(this);
    private DeleteStreamDb mDeleteStreamDb = new DeleteStreamDerby(this);
    private DistinctDb mDistinctDb = new DistinctDerby(this);
    private ExternalTablePollingStreamDb mExternalTablePollingStreamDb = new ExternalTablePollingStreamDerby(this);
    private ReplayStreamDb mReplayStreamDb = new ReplayStreamDerby(this);
    private GapWindowDb mGapWindowDb = new GapWindowDerby(this);
    private InsertStreamDb mInsertStreamDb = new InsertStreamDerby(this);
    private IntersectDb mIntersectDb = new IntersectDerby(this);
    private InvokeServiceDb mInvokeServiceDb = new InvokeServiceDerby(this);
    private InvokeStreamDb mInvokeStreamDb = new InvokeStreamDerby(this);
    private MergeDb mMergeDb = new MergeDerby(this);
    private MinusDb mMinusDb = new MinusDerby(this);
    private NotificationStreamDb mNotificationStreamDb = new NotificationStreamDerby(this);
    private PartitionedWindowDb mPartitionedWindowDb = new PartitionedWindowDerby(this);
    private RelationAggregatorDb mRelationAggregatorDb = new RelationAggregatorDerby(this);
    private RelationMapDb mRelationMapDb = new RelationMapDerby(this);
    private RelationStreamDb mRelationStreamDb = new RelationStreamDerby(this);
    private StreamInputDb mStreamInputDb = new StreamInputDerby(this);
    private StreamProjectionAndFilterDb mStreamProjectionAndFilterDb = new StreamProjectionAndFilterDerby(this);
    private TimeBasedAggregatorDb mTimeBasedAggregatorDb = new TimeBasedAggregatorDerby(this);
    private TimeBasedWindowDb mTimeBasedWindowDb = new TimeBasedWindowDerby(this);
    private TupleBasedAggregatorDb mTupleBasedAggregatorDb = new TupleBasedAggregatorDerby(this);
    private TupleSerialCorrelationDb mTupleSerialCorrelationDb = new TupleSerialCorrelationDerby(this);
    private UnionAllDb mUnionAllDb = new UnionAllDerby(this);
    private UnionDb mUnionDb = new UnionDerby(this);
    
    
    public static String getString(String[] tokens, String delim) {
        StringBuffer sb = new StringBuffer();
        int n = tokens.length - 1;
        if (n < 0) {
            return "";
        }
        for (int i = 0; i < n; i++) {
            sb.append(tokens[i]);
            sb.append(delim);
        }
        sb.append(tokens[n]);
        return sb.toString();
    }

    /**
     * Creates a new instance of DerbySpecial
     */
    public DerbySpecial(String iepDerbyJarPath, String dbSchemaName) {
        mIepDerbyJarPath = iepDerbyJarPath;
        mDbSchemaName = dbSchemaName;
    }

    private void installStoredProcedureJar(Connection con) throws Exception {
        if (mIepDerbyJarPath == null) {
            throw new Exception(mMessages.getString("DerbySpecial.Undefined_property", PROP_IEP_DERBY_JAR_PATH));
        }

        if (mDbSchemaName == null) {
            throw new Exception(mMessages.getString("DerbySpecial.Undefined_property", PROP_DB_SCHEMA));
        }
        StringBuffer sb = new StringBuffer();
        sb.append("{CALL sqlj.install_jar(");
        sb.append("'" + mIepDerbyJarPath + "', ");
        sb.append("'" + mDbSchemaName + ".IepStoredProcedures" + "', ");
        sb.append("0)}");
        String sqlStr = sb.toString();
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_register_IEP_stored_procedures", e);
        } finally {
            Util.close(s);
        }

        sb = new StringBuffer();
        sb.append("{CALL SYSCS_UTIL.SYSCS_SET_DATABASE_PROPERTY(");
        sb.append("'derby.database.classpath', ");
        sb.append("'" + mDbSchemaName + ".IepStoredProcedures" + "')}");
        sqlStr = sb.toString();
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_enable_IEP_stored_procedures_on_db_classpath", e);
        } finally {
            Util.close(s);
        }
    }

    private void uninstallStoredProcedureJar(Connection con) throws Exception {
        if (mDbSchemaName == null) {
            throw new Exception(mMessages.getString("DerbySpecial.Undefined_property", PROP_DB_SCHEMA));
        }
        StringBuffer sb = new StringBuffer();
        sb.append("{CALL sqlj.remove_jar(");
        sb.append("'" + mDbSchemaName + ".IepStoredProcedures" + "', ");
        sb.append("0)}");
        String sqlStr = sb.toString();

        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_unregister_IEP_stored_procedures", e);
        } finally {
            Util.close(s);
        }
    }

    public void createStoredProcedures(Connection con) throws Exception {
        installStoredProcedureJar(con);

        // user defined functions
        createDiffInDays(con);
        createDiffInMilliseconds(con);
        createCurrentTimestampNs(con);
        
        // stored procedures
        createCleanRelationByMinUsageTimeSP(con);
        mAttributeBasedWindowDb.createStoredProcedures(con);
        mContiguousOrderDb.createStoredProcedures(con);
        mDeleteStreamDb.createStoredProcedures(con);
        mDistinctDb.createStoredProcedures(con);
        mInsertStreamDb.createStoredProcedures(con);
        mIntersectDb.createStoredProcedures(con);
        mMergeDb.createStoredProcedures(con);
        mMinusDb.createStoredProcedures(con);
        mNotificationStreamDb.createStoredProcedures(con);
        mRelationStreamDb.createStoredProcedures(con);
        mRelationMapDb.createStoredProcedures(con);
        mRelationAggregatorDb.createStoredProcedures(con);
        mTupleBasedAggregatorDb.createStoredProcedures(con);
        mTimeBasedAggregatorDb.createStoredProcedures(con);
        mUnionAllDb.createStoredProcedures(con);
        mUnionDb.createStoredProcedures(con);
    }

    public void dropStoredProcedures(Connection con) throws Exception {
        // user defined functions
        dropDiffInMilliseconds(con);
        dropDiffInDays(con);
        dropCurrentTimestampNs(con);
        
        // stored procedures
        dropCleanRelationByMinUsageTimeSP(con);
        mAttributeBasedWindowDb.dropStoredProcedures(con);
        mContiguousOrderDb.dropStoredProcedures(con);
        mDeleteStreamDb.dropStoredProcedures(con);
        mDistinctDb.dropStoredProcedures(con);
        mInsertStreamDb.dropStoredProcedures(con);
        mIntersectDb.dropStoredProcedures(con);
        mMergeDb.dropStoredProcedures(con);
        mMinusDb.dropStoredProcedures(con);
        mNotificationStreamDb.dropStoredProcedures(con);
        mRelationAggregatorDb.dropStoredProcedures(con);
        mRelationStreamDb.dropStoredProcedures(con);
        mRelationMapDb.dropStoredProcedures(con);
        mTupleBasedAggregatorDb.dropStoredProcedures(con);
        mTimeBasedAggregatorDb.dropStoredProcedures(con);
        mUnionAllDb.dropStoredProcedures(con);
        mUnionDb.dropStoredProcedures(con);
        uninstallStoredProcedureJar(con);
    }

    private static void createDiffInMilliseconds(Connection con) throws Exception {
        StringBuffer sb = new StringBuffer();
        sb.append("CREATE FUNCTION DIFF_IN_MILLISECONDS(T1 TIMESTAMP, T2 TIMESTAMP)");
        sb.append(" RETURNS BIGINT");
        sb.append(" PARAMETER STYLE JAVA");
        sb.append(" NO SQL LANGUAGE JAVA");
        sb.append(" EXTERNAL NAME 'com.sun.jbi.engine.iep.core.derby.IEPFunctions.diffInMilliseconds'");
        String sqlStr = sb.toString();
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_create_function", "DIFF_IN_MILLISECONDS", e);
        } finally {
            Util.close(s);
        }
    }
    
    private static void dropDiffInMilliseconds(Connection con) throws Exception {
        StringBuffer sb = new StringBuffer();
        sb.append("DROP FUNCTION DIFF_IN_MILLISECONDS");
        String sqlStr = sb.toString();
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_drop_function", "CURRENT_TIMESTAMP_NS", e);
        } finally {
            Util.close(s);
        }
    }

    private static void createDiffInDays(Connection con) throws Exception {
        StringBuffer sb = new StringBuffer();
        sb.append("CREATE FUNCTION DIFF_IN_DAYS(D1 DATE, D2 DATE)");
        sb.append(" RETURNS BIGINT");
        sb.append(" PARAMETER STYLE JAVA");
        sb.append(" NO SQL LANGUAGE JAVA");
        sb.append(" EXTERNAL NAME 'com.sun.jbi.engine.iep.core.derby.IEPFunctions.diffInDays'");
        String sqlStr = sb.toString();
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_create_function", "DIFF_IN_DAYS", e);
        } finally {
            Util.close(s);
        }
    }

    private static void dropDiffInDays(Connection con) throws Exception {
        StringBuffer sb = new StringBuffer();
        sb.append("DROP FUNCTION DIFF_IN_DAYS");
        String sqlStr = sb.toString();
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_drop_function", "CURRENT_TIMESTAMP_NS", e);
        } finally {
            Util.close(s);
        }
    }

    private static void createCurrentTimestampNs(Connection con) throws Exception {
        StringBuffer sb = new StringBuffer();
        sb.append("CREATE FUNCTION CURRENT_TIMESTAMP_NS ()");
        sb.append(" RETURNS TIMESTAMP");
        sb.append(" PARAMETER STYLE JAVA");
        sb.append(" NO SQL LANGUAGE JAVA");
        sb.append(" EXTERNAL NAME 'com.sun.jbi.engine.iep.core.derby.IEPFunctions.currentTimestampWithNanoseconds'");
        String sqlStr = sb.toString();
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_create_function", "CURRENT_TIMESTAMP_NS", e);
        } finally {
            Util.close(s);
        }
    }
    
    private static void dropCurrentTimestampNs(Connection con) throws Exception {
        StringBuffer sb = new StringBuffer();
        sb.append("DROP FUNCTION CURRENT_TIMESTAMP_NS");
        String sqlStr = sb.toString();
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_drop_function", "CURRENT_TIMESTAMP_NS", e);
        } finally {
            Util.close(s);
        }
    }
    
    private static void createCleanRelationByMinUsageTimeSP(Connection con) throws Exception {
        StringBuffer sb = new StringBuffer();
        sb.append("CREATE PROCEDURE cleanRelationByMinUsageTime (");
        sb.append(" IN planId VARCHAR(20),");
        sb.append(" IN tableName VARCHAR(30)");
        sb.append(")");
        sb.append(" PARAMETER STYLE JAVA");
        sb.append(" MODIFIES SQL DATA");
        sb.append(" LANGUAGE JAVA");
        sb.append(" EXTERNAL NAME 'com.sun.jbi.engine.iep.core.derby.IEPStoredProcedures.cleanRelationByMinUsageTime'");
        String sqlStr = sb.toString();
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_create_procedure", "cleanRelationByMinUsageTime", e);
        } finally {
            Util.close(s);
        }
    }

    private static void dropCleanRelationByMinUsageTimeSP(Connection con) throws Exception {
        StringBuffer sb = new StringBuffer();
        sb.append("DROP PROCEDURE cleanRelationByMinUsageTime");
        String sqlStr = sb.toString();
        Statement s = null;
        try {
            s = con.createStatement();
            s.execute(sqlStr);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "DerbySpecial.Fail_to_drop_procedure", "cleanRelationByMinUsageTime", e);
        } finally {
            Util.close(s);
        }
    }

    public PreparedStatement createCleanRelationByMinUsageTimeStmt(Connection con, String planId, String tableName) throws Exception {
        StringBuffer sb = new StringBuffer();
        sb.append("{CALL cleanRelationByMinUsageTime(");
        sb.append("'" + planId + "', ");
        sb.append("'" + tableName + "')}");
        String sqlStr = sb.toString();
        return con.prepareCall(sqlStr);
    }

    public String getNativeSqlType(String jdbcType) {
        // No change for following types
        // SQL_TYPE_CHAR
        // SQL_TYPE_INTEGER
        // SQL_TYPE_BIGINT
        // SQL_TYPE_FLOAT
        // SQL_TYPE_DOUBLE
        // SQL_TYPE_VARCHAR
        // SQL_TYPE_DATE
        // SQL_TYPE_TIME
        // SQL_TYPE_TIMESTAMP
        return jdbcType;
    }
    
    public void createTable(Connection con, String tableName, Schema schema) throws Exception {
        List<ColumnMetadata> tailColumns = new ArrayList<ColumnMetadata>();
        createTableWithTailColumnsAndConstraints(con, tableName, schema, tailColumns, Collections.EMPTY_LIST);
    }
    
    public void createStream(Connection con, String tableName, Schema schema, List<ColumnMetadata> extraColumns, boolean autoGenIfPossible) throws Exception {
        if (autoGenIfPossible) {
            extraColumns.add(new ColumnMetadata(COL_SEQID, "BIGINT NOT NULL GENERATED BY DEFAULT AS IDENTITY (START WITH 1, INCREMENT BY 1)"));
        } else {
            extraColumns.add(new ColumnMetadata(COL_SEQID, "BIGINT"));
        }
        extraColumns.add(new ColumnMetadata(COL_TIMESTAMP, SQL_TYPE_TIMESTAMP));
        createTableWithTailColumnsAndConstraints(con, tableName, schema, extraColumns, Collections.EMPTY_LIST);
    }

    public void createRelation(Connection con, String tableName, Schema schema, List<ColumnMetadata> extraColumns, boolean autoGenIfPossible) throws Exception {
        if (autoGenIfPossible) {
            extraColumns.add(new ColumnMetadata(COL_SEQID, "BIGINT NOT NULL GENERATED BY DEFAULT AS IDENTITY (START WITH 1, INCREMENT BY 1)"));
        } else {
            extraColumns.add(new ColumnMetadata(COL_SEQID, "BIGINT"));
        }
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
                if (SQL_TYPE_VARCHAR.equals(columnType)) {
                    // VARCHAR[(size default 100)]
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
                sql.append(getNativeSqlType(columnType));
                if (SQL_TYPE_VARCHAR.equals(columnType)) {
                    // VARCHAR[(size default 100)]
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
            sql.setCharAt(sql.length() - 1, ')'); // replace the last ", "
            stmt = con.createStatement();
            stmt.execute(sql.toString());
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "DerbySpecial.Create_table_with", sql.toString());
            }
        } catch (Exception e) {
            throw new Exception(mMessages.getString("DerbySpecial.Fail_to_create_table_with", sql.toString()), e);
        } finally {
            Util.close(stmt);
        }
    }

    public void createSequence(Connection con, Operator op) throws Exception {
    }

    public void dropSequence(Connection con, Operator op) throws Exception {
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
