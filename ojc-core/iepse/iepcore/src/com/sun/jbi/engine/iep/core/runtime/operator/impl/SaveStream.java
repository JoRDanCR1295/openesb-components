package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.Timestamp;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.naming.InitialContext;

import com.sun.jbi.engine.iep.core.runtime.operator.Inserter;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.util.ArrayList;

/**
 * 
 * @author radval
 *
 */
public class SaveStream extends AbstractOperator implements Inserter {

    private static final Messages mMessages = Messages.getMessages(AbstractOperator.class);
    private Logger mLogger = Logger.getLogger(SaveStream.class.getName());
    private String mDatabaseJndiName;
    private String mTableName;
    private boolean mIsPreserveTable;
    private Properties mConfigProp;
    private Connection mTargetDBConnection;

    public SaveStream(Map prop) {
        initialize(prop);

        mDatabaseJndiName = (String) prop.get(PROP_DATABASE_JNDI_NAME);
        mTableName = (String) prop.get(PROP_TABLE_NAME);
        mIsPreserveTable = PropertyUtil.getboolean(prop, PROP_IS_PRESERVE_TABLE, true);

        mConfigProp = (Properties) prop.get(PROP_CONFIG_PROPERTIES);
    }

    public String getOutputType() {
        return IO_TYPE_NONE;
    }

    @Override
    public void setRuntimeConnection(Connection con) {
        super.setRuntimeConnection(con);
    }

    @Override
    public void unsetRuntimeConnection() {
        super.unsetRuntimeConnection();

        //drop the save stream connection cached here
        cleanup();
    }

    @Override
    public void deployExternalResource() throws Exception {
        Connection saveStreamConnection = null;
        try {
            saveStreamConnection = getSaveStreamConnection();

            String tableName = getTableName();
            Schema schema = getOutputSchema();
            Set columnNamesToIgnore = new HashSet();
            columnNamesToIgnore.add(COL_SEQID);
            columnNamesToIgnore.add(COL_TIMESTAMP);

            DbSpecial targetDbSpecial = Util.getDbSpecial(saveStreamConnection, mConfigProp);
            int status = targetDbSpecial.checkTableStatus(saveStreamConnection, null, tableName, schema, columnNamesToIgnore);

            if (isPreserveTable()) {
                switch (status) {
                    case TS_UNKNOWN:
                        dropSaveStreamTable(saveStreamConnection);
                        createSaveStreamTable(saveStreamConnection);
                        break;
                    case TS_NAME_NOT_EXIST:
                        createSaveStreamTable(saveStreamConnection);
                        break;
                    case TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA:
                        dropSaveStreamTable(saveStreamConnection);
                        createSaveStreamTable(saveStreamConnection);
                        break;
//                    case TS_TABLE_EXIST:
//                    	dropSaveStreamTable(saveStreamConnection);
//                    	createSaveStreamTable(saveStreamConnection);
//                    	break;
                }
            } else {
                switch (status) {
                    case TS_UNKNOWN:
                        dropSaveStreamTable(saveStreamConnection);
                        createSaveStreamTable(saveStreamConnection);
                        break;
                    case TS_NAME_NOT_EXIST:
                        createSaveStreamTable(saveStreamConnection);
                        break;
                    case TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA:
                        dropSaveStreamTable(saveStreamConnection);
                        createSaveStreamTable(saveStreamConnection);
                        break;

                    case TS_TABLE_EXIST:
                        dropSaveStreamTable(saveStreamConnection);
                        createSaveStreamTable(saveStreamConnection);
                        break;
                }

            }

        } catch (Exception ex) {
            String err = mMessages.getString("SaveStream.Failed_to_create_table", getTableName());
            mLogger.log(Level.SEVERE, err, ex);
            throw ex;
        } finally {
        	//close connection
            Util.close(saveStreamConnection);
        }

    }

    @Override
    public void undeployExternalResource() {
        if (!isPreserveTable()) {
            Connection saveStreamConnection = null;
            try {
                saveStreamConnection = getSaveStreamConnection();
                dropSaveStreamTable(saveStreamConnection);
                Util.close(saveStreamConnection);
            } catch (Exception ex) {
                mMessages.log(Level.SEVERE, "SaveStream.fail_to_undeployExternalResource", getName(), ex);
            } finally {
                Util.close(saveStreamConnection);
            }
        }
    }

    @Override
    protected void dropOutputQueue(Connection con) throws Exception {
    }

    @Override
    protected void createOutputQueue(Connection con) throws Exception {
    }
    
    @Override
    protected void dropSynopsis(Connection con) throws Exception {
    }

    public void createSaveStreamTable(Connection con) throws Exception {
        Schema schema = getOutputSchema();
        String tableName = getTableName();
        DbSpecial targetDbSpecial = Util.getDbSpecial(con, mConfigProp);
        if (targetDbSpecial != null) {
            targetDbSpecial.createStream(con, tableName, schema, new ArrayList<ColumnMetadata>(), true);
        }
    }

    public void dropSaveStreamTable(Connection con) throws Exception {
        String tableName = getTableName();
        Util.dropTable(con, tableName);
    }

    public String getDatabaseJndiName() {
        return mDatabaseJndiName;
    }

    public String getTableName() {
        return mTableName;
    }

    public boolean isPreserveTable() {
        return mIsPreserveTable;
    }

    protected void executeOperateStmt(Timestamp prevT, Timestamp curT) throws Exception {
        if (mOperateStmt != null) {
            mOperateStmt[0].setObject(1, prevT);
            mOperateStmt[0].setObject(2, curT);
            ResultSet rs = mOperateStmt[0].executeQuery();
            while (rs.next()) {
                int counter = 1;
                ResultSetMetaData rsMetaData = rs.getMetaData();
                int columnCount = rsMetaData.getColumnCount();
                for (int i = 0; i < columnCount; i++) {
                    Object val = Util.getTypeSpecificValue(rs, counter);
                    mOperateStmt[1].setObject(counter, val);
                    counter++;
                }

                mOperateStmt[1].executeUpdate();
            }
        }
    }

    @Override
    protected void createOperateStmt(Connection con) throws Exception {
        //create and store a save stream connection 
        //for operator processing this connection is long lived similar to con
        //passed in and gets closed in unsetRuntimeConnection

    	//call clean up since when su is stopped and
        //restarted we get same plan with same old
        //external polling table which may have
        //old connection and lastFetched record etc

        cleanup();
        mTargetDBConnection = getSaveStreamConnection();

        Connection source = con;
        mOperateStmt = createOperateStatements(source, mTargetDBConnection, this);
    }

    private PreparedStatement[] createOperateStatements(Connection source, Connection target, SaveStream op) throws Exception {
        PreparedStatement[] statements = null;

        //select from stream input
        PreparedStatement insertStmt = createTargetInsertStatement(target, op);
        StringBuffer selectBuf = new StringBuffer();
        if(op.getInputOperatorList().size() > 0) {
            selectBuf.append("SELECT ");
            Operator input =  op.getInputOperatorList().get(0);
            Schema s = input.getOutputSchema();
            for (int i = 0, I = s .getColumnCount(); i < I; i++) {
                ColumnMetadata cmd = s .getColumnMetadata(i);
                selectBuf.append(cmd.getColumnName() + ",");
            }
            
            selectBuf.append(COL_SEQID);
            selectBuf.append(",");            
            
            selectBuf.append(COL_TIMESTAMP);
            selectBuf.append(" FROM ");
            selectBuf.append(input.getQueueName());
            
            
            selectBuf.append(" WHERE ? < ");
            selectBuf.append(COL_TIMESTAMP + " AND ");
            selectBuf.append(COL_TIMESTAMP + " <= ?");
        }
        
        String selectSqlStr = selectBuf.toString();
        PreparedStatement selectStmt = source.prepareStatement(selectSqlStr);
        statements = new PreparedStatement[] {selectStmt, insertStmt};
        return statements;
    }
    
    private PreparedStatement createTargetInsertStatement(Connection target, SaveStream op) throws Exception {
        PreparedStatement statement = null;
        
        String tableName = op.getTableName();
        Schema schema = op.getOutputSchema();
        StringBuffer insertBuf = new StringBuffer();
        
        //insert into save stream table
        insertBuf.append("INSERT INTO ");
        insertBuf.append(tableName);
        insertBuf.append("(");
        for (int i = 0, I = schema.getColumnCount(); i < I; i++) {
            ColumnMetadata cmd = schema.getColumnMetadata(i);
            insertBuf.append(cmd.getColumnName() + ",");
        }
        
        insertBuf.append(COL_SEQID);
        insertBuf.append(",");
        insertBuf.append(COL_TIMESTAMP);
        insertBuf.append(") ");
        
        insertBuf.append(" VALUES(");
        for (int i = 0, I = schema.getColumnCount(); i < I; i++) {
          insertBuf.append("?,");
        }
        
        //ems_seqid and ems_timestamp
        insertBuf.append("?,?)");
        
        String insertSqlStr = insertBuf.toString();
        statement = target.prepareStatement(insertSqlStr);
        
        return statement;
    }    
    
    public PreparedStatement getInsertStatement(Connection con, boolean includeDBCommandForTS) throws Exception {
        return null;
    }

    private Connection getSaveStreamConnection() throws Exception {
        Connection conn = null;
        String databaseJndiName = getDatabaseJndiName();

        try {
            InitialContext jndiContext = (InitialContext) mConfigProp.get(PROP_DB_JNDI_CONTEXT);
            if (jndiContext != null) {
                conn = Util.getConnection(jndiContext, databaseJndiName);
            }
            
        } catch (Exception ex) {
            String err = mMessages.getString("SaveStream.Fail_to_get_connection_for_jndiName", databaseJndiName);
            mLogger.log(Level.SEVERE, err);
            throw new Exception(ex);
        }
        return conn;
    }

    private void closeStatement() {
        if (mOperateStmt != null) {
            for (int i = 0; i < mOperateStmt.length; i++) {
                Util.close(mOperateStmt[i]);
                mOperateStmt[i] = null;
            }

            mOperateStmt = null;
        }
    }

    private void cleanup() {
        closeStatement();

        //drop the save stream connection cached here
        if (mTargetDBConnection != null) {
            Util.close(mTargetDBConnection);
        }
    }
}    
        

