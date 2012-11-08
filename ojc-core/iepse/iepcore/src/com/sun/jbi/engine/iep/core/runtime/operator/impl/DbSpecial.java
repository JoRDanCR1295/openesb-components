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
 * @(#)DbSpecial.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.List;
import java.util.Set;

/*
 * DbSpecial.java
 *
 * Created on August 22, 2005, 12:55 PM
 *
 * Interface to abstract certain database specific sql
 *
 * @author Bing Lu
 */

public interface DbSpecial extends OperatorConstants {
    public void createStoredProcedures(Connection con) throws Exception;
    public void dropStoredProcedures(Connection con) throws Exception;
    
    // DELETE FROM TableName Alias ...
    // vs
    // DELETE FROM TableName ...
    public PreparedStatement createCleanRelationByMinUsageTimeStmt(Connection con, String planId, String tableName) throws Exception;
    
    // 1. Sequence vs Auto-generated Identity
    //
    // 2. NUMERIC vs BIGINT
    //
    // 3. For RelationStram only:
    //    INSERT INTO S (....) SELECT ... FROM R WHERE ... ORDER BY ..
    //    vs
    //    INSERT INTO S (....) SELECT ... FROM R WHERE ...
    //    That is no support for 'ORDER BY' in the SELECT expression nested in an INSERT statement
    //
    // 4. For TimeBasedAggregator only:
    //    JDBC driver support ? in selection-list of SELECT statement (SELECT ? as Name FROM S1)
    //    vs
    //    JDBC driver doesn't support ? in selection-list of SELECT
    //
    // 5. For TupleBasedAggregator only:
    //    GROUP BY Expression vs GROUP BY ColumnName
    public String getNativeSqlType(String jdbcType);
    public void createTable(Connection con, String tableName, Schema schema) throws Exception;
    public void createStream(Connection con, String tableName, Schema schema, List<ColumnMetadata> extraColumns, boolean autoGenIfPossible) throws Exception;
    public void createRelation(Connection con, String tableName, Schema schema, List<ColumnMetadata> extraColumns, boolean autoGenIfPossible) throws Exception;
    public void createTableWithTailColumnsAndConstraints(Connection con, String tableName, Schema schema, List<ColumnMetadata> tailColumns,java.util.List<String> constraints) throws Exception;
    public void dropTable(String tableName, Connection con) throws Exception;
    public void createSequence(Connection con, Operator op) throws Exception;
    public void dropSequence(Connection con, Operator op) throws Exception;
    public void createIndex(Connection con, String indexName, String tableName, List<String> columnNameList) throws Exception;
    public void dropIndex(Connection con, String indexName, String tableName) throws Exception;
    public int checkTableStatus(Connection con, String dbSchema, String tableName, Schema schema, Set columnNamesToIgnore);
    public boolean hasTable( Connection conn,  String dbSchema, String tableName) throws Exception;

    public AttributeBasedWindowDb getAttributeBasedWindowDb();
    public ContiguousOrderDb getContiguousOrderDb();
    public DeleteStreamDb getDeleteStreamDb();
    public DistinctDb getDistinctDb();
    public ExternalTablePollingStreamDb getExternalTablePollingStreamDb();
    public ReplayStreamDb getReplayStreamDb();
    public GapWindowDb getGapWindowDb();
    public InsertStreamDb getInsertStreamDb();
    public IntersectDb getIntersectDb();
    public InvokeServiceDb getInvokeServiceDb();
    public InvokeStreamDb getInvokeStreamDb();
    public MergeDb getMergeDb();
    public MinusDb getMinusDb();
    public NotificationStreamDb getNotificationStreamDb();
    public PartitionedWindowDb getPartitionedWindowDb();
    public RelationAggregatorDb getRelationAggregatorDb();
    public RelationMapDb getRelationMapDb();
    public RelationStreamDb getRelationStreamDb();
    public StreamInputDb getStreamInputDb();
    public StreamProjectionAndFilterDb getStreamProjectionAndFilterDb();
    public TimeBasedAggregatorDb getTimeBasedAggregatorDb();
    public TimeBasedWindowDb getTimeBasedWindowDb();
    public TupleBasedAggregatorDb getTupleBasedAggregatorDb();
    public TupleSerialCorrelationDb getTupleSerialCorrelationDb();
    public UnionAllDb getUnionAllDb();
    public UnionDb getUnionDb();

    public PreparedStatement getRelationResultForDebugInfo(Connection con,String inputTableName,String[] cols) throws Exception ;
    
}