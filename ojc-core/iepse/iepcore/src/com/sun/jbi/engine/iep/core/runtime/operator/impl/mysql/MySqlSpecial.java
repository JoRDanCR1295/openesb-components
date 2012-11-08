package com.sun.jbi.engine.iep.core.runtime.operator.impl.mysql;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.AbstractDbSpecial;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.AttributeBasedWindowDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.ContiguousOrderDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.DeleteStreamDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.DistinctDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.ExternalTablePollingStreamDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.GapWindowDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.InsertStreamDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.IntersectDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.InvokeServiceDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.InvokeStreamDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.MergeDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.MinusDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.NotificationStreamDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.PartitionedWindowDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.RelationAggregatorDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.RelationMapDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.RelationStreamDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.ReplayStreamDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.StreamInputDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.StreamProjectionAndFilterDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.TimeBasedAggregatorDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.TimeBasedWindowDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.TupleBasedAggregatorDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.TupleSerialCorrelationDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.UnionAllDb;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.UnionDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.Collections;
import java.util.Iterator;
import java.util.Set;
import java.util.logging.Level;

public class MySqlSpecial extends AbstractDbSpecial {
    private static final Messages mMessages = Messages.getMessages(MySqlSpecial.class);

    private AttributeBasedWindowDb mAttributeBasedWindowDb = new AttributeBasedWindowMySql(this);
    private ContiguousOrderDb mContiguousOrderDb = new ContiguousOrderMySql(this);
    private DistinctDb mDistinctDb = new DistinctMySql(this);
    private ExternalTablePollingStreamDb mExternalTablePollingStreamDb = new ExternalTablePollingStreamMySql(this);
    private GapWindowDb mGapWindowDb = new GapWindowMySql(this);
    private DeleteStreamDb mDeleteStreamDb = new DeleteStreamMySql(this);    
    private InsertStreamDb mInsertStreamDb = new InsertStreamMySql(this);
    private IntersectDb mIntersectDb = new IntersectMySql(this);
    private InvokeServiceDb mInvokeServiceDb = new InvokeServiceMySql(this);
    private InvokeStreamDb mInvokeStreamDb = new InvokeStreamMySql(this);
    private MergeDb mMergeDb = new MergeMySql(this);
    private MinusDb mMinusDb = new MinusMySql(this);
    private NotificationStreamDb mNotificationStreamDb = new NotificationStreamMySql(this);
    private PartitionedWindowDb mPartitionedWindowDb = new PartitionedWindowMySql(this);
    private RelationMapDb mRelationMapDb = new RelationMapMySql(this);
    private RelationAggregatorDb mRelationAggregatorDb = new RelationAggregatorMySql(this);        
    private RelationStreamDb mRelationStreamDb = new RelationStreamMySql(this);
    private ReplayStreamDb mReplayStreamDb = new ReplayStreamMySql(this);
    private StreamInputDb mStreamInputDb = new StreamInputMySql(this);
    private StreamProjectionAndFilterDb mStreamProjectionAndFilterDb = new StreamProjectionAndFilterMySql(this);
    private TimeBasedAggregatorDb mTimeBasedAggregatorDb = new TimeBasedAggregatorMySql(this);    
    private TimeBasedWindowDb mTimeBasedWindowDb = new TimeBasedWindowMySql(this);    
    private TupleBasedAggregatorDb mTupleBasedAggregatorDb = new TupleBasedAggregatorMySql(this);
    private TupleSerialCorrelationDb mTupleSerialCorrelationDb = new TupleSerialCorrelationMySql(this);
    private UnionAllDb mUnionAllDb = new UnionAllMySql(this);
    private UnionDb mUnionDb = new UnionMySql(this);
    
    public MySqlSpecial() {
        super();
    }

        
    public PreparedStatement createCleanRelationByMinUsageTimeStmt(Connection con, String planId, String tableName) throws Exception {
        // DELETE FROM R1 t1
        // WHERE EXISTS (SELECT 'x' FROM R1 WHERE Tag='-' AND Row_Id = t1.Row_Id AND Timestamp <= min_usage_time)
        StringBuffer sb = new StringBuffer();
        sb.append("DELETE t1 FROM ");
        sb.append(tableName);
        sb.append(" t1, ");
        sb.append(tableName);
        sb.append(" t2 WHERE t2." + COL_TAG + " = '-' AND t2." + COL_SEQID + " = t1." + COL_SEQID);
        sb.append(" AND t2." + COL_TIMESTAMP + " <= (SELECT MIN(" + COL_TIMESTAMP + ") FROM ");
        sb.append(Util.getTableUsageTableName(planId));
        sb.append(" WHERE " + COL_TABLE_NAME + " = '" );
        sb.append(tableName);
        sb.append("')");
        String sqlStr = sb.toString();
        return con.prepareStatement(sqlStr);
    }
    
    public void createTable(Connection con, String tableName, Schema schema) throws Exception {
        List<ColumnMetadata> tailColumns = new ArrayList<ColumnMetadata>();
        createTableWithTailColumnsAndConstraints(con, tableName, schema, tailColumns, Collections.EMPTY_LIST);
    }

    public void createStream(Connection con, String tableName, Schema schema, List<ColumnMetadata> extraColumns, boolean autoGenIfPossible) throws Exception {
        List<String> constraints = new ArrayList<String>();
        if (autoGenIfPossible) {
            extraColumns.add(new ColumnMetadata(COL_SEQID, "BIGINT UNSIGNED NOT NULL AUTO_INCREMENT"));
            constraints.add("PRIMARY KEY (" + COL_SEQID + ")");
        } else {
            extraColumns.add(new ColumnMetadata(COL_SEQID, "BIGINT"));
        }
        extraColumns.add(new ColumnMetadata(COL_TIMESTAMP, SQL_TYPE_TIMESTAMP));
        createTableWithTailColumnsAndConstraints(con, tableName, schema, extraColumns, constraints);
    }

    public void createRelation(Connection con, String tableName, Schema schema, List<ColumnMetadata> extraColumns, boolean autoGenIfPossible) throws Exception {
        List<String> constraints = new ArrayList<String>();
        if (autoGenIfPossible) {
            extraColumns.add(new ColumnMetadata(COL_SEQID, "BIGINT UNSIGNED NOT NULL AUTO_INCREMENT"));
            constraints.add("PRIMARY KEY (" + COL_SEQID + "," + COL_TAG + ")");
        } else {
            extraColumns.add(new ColumnMetadata(COL_SEQID, "BIGINT"));
        }
        extraColumns.add(new ColumnMetadata(COL_TIMESTAMP, SQL_TYPE_TIMESTAMP));
        extraColumns.add(new ColumnMetadata(COL_TAG, SQL_TYPE_CHAR, 1));
        createTableWithTailColumnsAndConstraints(con, tableName, schema, extraColumns, constraints);
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
                mMessages.log(Level.FINE, "MySqlSpecial.Create_table_with", sql.toString());
            }
        } catch (Exception e) {
            throw new Exception(mMessages.getString("MySqlSpecial.Fail_to_create_table_with", sql.toString()), e);
        } finally {
            Util.close(stmt);
        }
    }
    
    public void dropTable(String tableName, Connection con) throws Exception {
        StringBuffer sql = new StringBuffer();
        sql.append("DROP TABLE ");
        sql.append(tableName);

        PreparedStatement ps = con.prepareStatement(sql.toString());

        ps.executeUpdate();
    }

    public void createSequence(Connection con, Operator op) throws Exception {
        // MySQL doesn't support sequence
    }

    public void dropSequence(Connection con, Operator op) throws Exception {
        // MySQL doesn't support sequence
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
            stmt.execute("DROP INDEX " + indexName + " ON " + tableName);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Fail_to_drop_index", indexName, e);
        } finally {
            Util.close(stmt);
        }
    }

    public int checkTableStatus(Connection con, String dbSchema, String tableName, Schema schema, Set columnNamesToIgnore) {
        PreparedStatement stmt = null;
        try {
            DatabaseMetaData dbmd = con.getMetaData();

            // check if tableName already exists
            //note that here we are NOT using database meta data
            //api. those api are anyway slow and creates a deadlock
            //in a situation as described below:

            //We use schema and
            //table name to check existence of a table.
            //this is required because in operators like save stream
            //where this api is called, if iep is running on a database
            //and save stream is saving on same database as iep
            //then a when using database metadata getTable table scan without specifying schema and table
            //results in a deadlock which is because
            //iep deploy process logic obtains a lock on EMS_PLAN table, see Token
            //class and as part of that call save stream again tries to
            //scan tables using a different connection which includes this EMS_PLAN so a deadlock
            //occur since Token is released only at the end of deploy
            //which releases lock on EMS_PLAN and save stream table scan using
            //data base meta data api waits for EMS_PLAN to be available
            boolean nameExist = hasTable(con, dbSchema, tableName);
            if (!nameExist) {
                return TS_NAME_NOT_EXIST;
            }

            // check if two table have the same schema
            //prepare dummy query
            String identifierQuotedString = con.getMetaData().getIdentifierQuoteString();
            identifierQuotedString = identifierQuotedString.trim();
            StringBuffer sqlBuf = new StringBuffer("SELECT * FROM ");
            //for mysql we always can safely use upper case
            //using lower case is not recognized as schema and table
            //name
            if (dbSchema != null) {
                sqlBuf.append(identifierQuotedString + dbSchema + identifierQuotedString + ".");
            }
            if (tableName != null) {
                sqlBuf.append(identifierQuotedString + tableName + identifierQuotedString);
            }
            sqlBuf.append(" WHERE 1 = 2");
            stmt = con.prepareStatement(sqlBuf.toString());
            stmt.executeQuery();

            //query is never executed on the server - only prepared
            ResultSetMetaData rsmd = stmt.getMetaData();
            int numcols = rsmd.getColumnCount();
            int columnCount = 0;
            for (int col = 0; col < numcols; col++) {
                String columnName = rsmd.getColumnName(col + 1);
                int type = rsmd.getColumnType(col + 1);

                boolean ignoreColumn = false;

                Iterator iterator = columnNamesToIgnore.iterator();
                while (iterator.hasNext()) {
                    String nameToIgnore = (String) iterator.next();
                    if (nameToIgnore.equalsIgnoreCase(columnName)) {
                        ignoreColumn = true;
                        break;
                    }
                }

                if (ignoreColumn) {
                    continue;
                }

                columnCount++;

                boolean foundName = false;
                ColumnMetadata clmd = null;

                for (int i = 0, I = schema.getColumnCount(); i < I; i++) {
                    clmd = schema.getColumnMetadata(i);
                    if (clmd.getColumnName().equalsIgnoreCase(columnName)) {
                        foundName = true;
                        break;
                    }
                }
                if (!foundName) {
                    return TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA;
                }

                if (Util.getSqlType(clmd.getColumnType()) != type) {
                    return TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA;
                }

                if (clmd.hasColumnSize()) {
                    int columnSize = rsmd.getPrecision(col + 1);
                    if (clmd.getColumnSize() != columnSize) {
                        return TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA;
                    }
                    if (clmd.hasColumnScale()) {
                        int columnScale = rsmd.getScale(col + 1);
                        if (clmd.getColumnScale() != columnScale) {
                            return TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA;
                        }
                    }
                }
            }
            if (columnCount != schema.getColumnCount()) {
                return TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA;
            }

            return TS_TABLE_EXIST;
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "AbstractDbSpecial.Fail_to_check_status_of_table", tableName, e);
        } finally {
            Util.close(stmt);
        }
        return TS_UNKNOWN;
    }

    public boolean hasTable(Connection conn, String dbSchema, String tableName) throws Exception {
        PreparedStatement stmt = null;
        ResultSet results = null;
        try {
            String identifierQuotedString = conn.getMetaData().getIdentifierQuoteString();
            identifierQuotedString = identifierQuotedString.trim();
            StringBuffer sqlBuf = new StringBuffer("SELECT 'x' FROM ");
            if (dbSchema != null) {
                sqlBuf.append(identifierQuotedString + dbSchema + identifierQuotedString + ".");
            }
            if (tableName != null) {
                sqlBuf.append(identifierQuotedString + tableName + identifierQuotedString);
            }
            sqlBuf.append(" WHERE 1 = 2");
            stmt = conn.prepareStatement(sqlBuf.toString());
            results = stmt.executeQuery();
            return true;  // if table does exist, no rows will ever be returned
        } catch (SQLException e) {
            return false;  // if table does not exist, an exception will be thrown
        } finally {
            if (results != null) {
                results.close();
            }
            if (stmt != null) {
                stmt.close();
            }
        }
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

        // Change for following types
        if (SQL_TYPE_TIMESTAMP.equals(jdbcType)) {
            return "DATETIME";
        }
        if (SQL_TYPE_CLOB.equals(jdbcType)) {
            return "LONGTEXT";
        }
        return jdbcType;
    }
    
    public void createStoredProcedures(Connection con) throws Exception {
    }
    
    public void dropStoredProcedures(Connection con) throws Exception {
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
    
    public DistinctDb getDistinctDb(){
        return mDistinctDb;
    }
    
    public ExternalTablePollingStreamDb getExternalTablePollingStreamDb(){
        return mExternalTablePollingStreamDb;
    }
    
    public ReplayStreamDb getReplayStreamDb(){
        return mReplayStreamDb;
    }
    
    public GapWindowDb getGapWindowDb(){
        return mGapWindowDb;
    }
    
    public InsertStreamDb getInsertStreamDb(){
        return mInsertStreamDb;
    }
    
    public IntersectDb getIntersectDb(){
        return mIntersectDb;
    }
    
    public InvokeServiceDb getInvokeServiceDb(){
        return mInvokeServiceDb;
    }

    public InvokeStreamDb getInvokeStreamDb() {
        return mInvokeStreamDb;
    }

    public MergeDb getMergeDb() {
        return mMergeDb;
    }

    public MinusDb getMinusDb(){
        return mMinusDb;
    }

    public NotificationStreamDb getNotificationStreamDb(){
        return mNotificationStreamDb;
    }
    
    public PartitionedWindowDb getPartitionedWindowDb(){
        return mPartitionedWindowDb;
    }

    public RelationAggregatorDb getRelationAggregatorDb(){
        return mRelationAggregatorDb;
    }

    public RelationMapDb getRelationMapDb(){
        return mRelationMapDb;
    }

    public RelationStreamDb getRelationStreamDb(){
        return mRelationStreamDb;
    }

    public StreamInputDb getStreamInputDb() {
        return mStreamInputDb;
    }

    public StreamProjectionAndFilterDb getStreamProjectionAndFilterDb() {
        return mStreamProjectionAndFilterDb;
    }
    
    public TimeBasedAggregatorDb getTimeBasedAggregatorDb(){
        return mTimeBasedAggregatorDb;
    }

    public TimeBasedWindowDb getTimeBasedWindowDb() {
        return mTimeBasedWindowDb;
    }    
    
    public TupleBasedAggregatorDb getTupleBasedAggregatorDb() {
        return mTupleBasedAggregatorDb;
    }

    public TupleSerialCorrelationDb getTupleSerialCorrelationDb(){
        return mTupleSerialCorrelationDb;
    }

    public UnionAllDb getUnionAllDb(){
        return mUnionAllDb;
    }

    public UnionDb getUnionDb(){
        return mUnionDb;
    }

    public PreparedStatement getRelationResultForDebugInfo(Connection con,String inputTableName,String[] cols) throws Exception {
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
 }
