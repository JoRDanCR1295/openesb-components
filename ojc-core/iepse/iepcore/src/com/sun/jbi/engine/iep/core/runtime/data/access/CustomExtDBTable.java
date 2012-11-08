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
package com.sun.jbi.engine.iep.core.runtime.data.access;

import com.sun.jbi.engine.iep.core.runtime.data.access.DataAccessTabularInfo;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.AbstractOperator;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.DbSpecial;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.Iterator;
import java.util.List;

/**
 *
 * @author rdwivedi
 */
public class CustomExtDBTable {
    private String mTableName = "ExtTable";
    PreparedStatement mStmt  = null;
    PreparedStatement mDltStmt = null;
    private static final Messages mMessages = Messages.getMessages(CustomExtDBTable.class);
    public CustomExtDBTable() {
        
    }
    public void init(Operator opr , Connection con) throws Exception {
       mTableName = opr.getExtTable();
       createTableIfReq(opr,con);
       getPreparedStatement(con,opr);
    }
    
    private PreparedStatement getPreparedStatement(Connection con, Operator opr) throws Exception {
        mDltStmt = con.prepareStatement("DELETE from " + mTableName);

        StringBuffer insertSQL = new StringBuffer();
        insertSQL.append("INSERT INTO ");
        insertSQL.append(mTableName);
        insertSQL.append(" VALUES (");
        for (int i = 0, I = opr.getOutputSchema().getColumnCount() ; i < I -1; i++) {
            insertSQL.append("?,");
        }
        insertSQL.append("?");
        insertSQL.append(")");
        mStmt = con.prepareStatement(insertSQL.toString());
        return mStmt;
    }
    private void createTableIfReq(Operator opr , Connection con) throws Exception {
        DbSpecial dbSpecial = ((AbstractOperator)opr).getDBSpecial();
        try {
        dbSpecial.createTable(con, mTableName, opr.getOutputSchema());
        } catch(Exception e) {
            // do nothing later handle created table scenario as described below.
        }
        /*
        int status = Util.checkTableStatus(con, mDbSchemaName, tableName, mActualOutputSchema);
            switch (status) {
                case OperatorConstants.TS_UNKNOWN:
                    Util.dropTable(con, tableName);
                    dbSpecial.createTable(con, tableName, mActualOutputSchema);
                    break;
                case OperatorConstants.TS_NAME_NOT_EXIST:
                    dbSpecial.createTable(con, tableName, mActualOutputSchema);
                    break;
                case OperatorConstants.TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA:
                    Util.dropTable(con, tableName);
                    dbSpecial.createTable(con, tableName, mActualOutputSchema);
                    break;
                case OperatorConstants.TS_TABLE_EXIST:
                    Util.cleanTable(con, tableName);
                    break;
            }  
         */ 
    }
    
    public void execute(Operator opr , Connection con , DataAccessTabularInfo tabularData) throws Exception {
        //Util.cleanTable(con, mTableName);
        
        List l  = tabularData.getData();
        if(l.size()>0) {
            mDltStmt.executeUpdate();
        }
        Iterator iter = l.iterator();
        while(iter.hasNext()) {
            List row = (List)iter.next();
            for (int i = 0, I = opr.getOutputSchema().getColumnCount() ; i < I ; i++) {
                mStmt.setObject(i+1, row.get(i));
            }
            mStmt.executeUpdate(); 
        }
           
        }
        
        
    
}
