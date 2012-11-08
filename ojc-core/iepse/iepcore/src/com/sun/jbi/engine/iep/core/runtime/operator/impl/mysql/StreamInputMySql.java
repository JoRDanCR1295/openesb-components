package com.sun.jbi.engine.iep.core.runtime.operator.impl.mysql;

import java.sql.Connection;
import java.sql.PreparedStatement;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.StreamInput;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.StreamInputDb;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;

public class StreamInputMySql implements StreamInputDb {
    private static final Messages mMessages = Messages.getMessages(StreamInputMySql.class);

    private MySqlSpecial mMySqlSpecial;

    public StreamInputMySql(MySqlSpecial mySqlSpecial) {
        mMySqlSpecial = mySqlSpecial;
    }

    public PreparedStatement createInsertStatement(Connection con, StreamInput op, boolean includeDBCommandForTS) throws Exception {
        String tableName = op.getQueueName();
        Schema schema = op.getOutputSchema();
        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(tableName);
        sb.append("(");
        for (int i = 0, I = schema.getColumnCount(); i < I; i++) {
            ColumnMetadata cmd = schema.getColumnMetadata(i);
            sb.append(cmd.getColumnName() + ",");
        }
        sb.append(COL_TIMESTAMP);
        sb.append(") VALUES (");
        for (int i = 0, I = schema.getColumnCount(); i < I; i++) {
            sb.append("?,");
        }
        if (includeDBCommandForTS) {
            //sb.append("CURRENT_TIMESTAMP");
            sb.append("CURRENT_TIMESTAMP");
        } else {
            sb.append("?");
        }
        sb.append(")");
        String sqlStr = sb.toString();
        PreparedStatement stmt = con.prepareStatement(sqlStr);
        return stmt;
    }
}
