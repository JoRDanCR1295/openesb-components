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


import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.share.SharedConstants;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

/**
 *
 * @author rdwivedi
 */
public class OperatorDA implements SharedConstants {
    
    DataAccessTabularInfo mTabularInfo  = null;
    public OperatorDA() {
        
    }
    
    public boolean execute(Operator opr ,Connection con) throws Exception {
        boolean result = false;
        mTabularInfo = null;
        if(opr.getOutputType().equals(IO_TYPE_RELATION)){
            mTabularInfo =  executeRelationOutput(opr,con);
            
        } else {
            mTabularInfo = getNewRowsAdded(opr,con);
        }
        if(mTabularInfo!=null) {
                result = true;
        } 
        return result;
        
    }

    public DataAccessTabularInfo getTabularData() {
        return mTabularInfo;
    }
    private DataAccessTabularInfo getNewRowsAdded(Operator opr,Connection con) throws Exception {
        String tabName = opr.getQueueName();
        //String qry = "Select * from "+ tabName ;//+ " where EMS_TAG='+'";
        //String qry2 = "Select * from " + tabName + " where EMS_TAG='+'";
        String qry2 = "Select * from " + tabName  ;
        Statement stmt = null;
        ResultSet rs = null;
        DataAccessTabularInfo dInfo = new DataAccessTabularInfo();
        try {
            stmt = con.createStatement();
            rs = stmt.executeQuery(qry2);
            ResultSetMetaData md = rs.getMetaData();
            int cCount = md.getColumnCount();
            ArrayList<String> cList = new ArrayList<String>();
            for (int i = 1; i <= cCount; i++) {
                cList.add(md.getColumnName(i));
            }
            dInfo.setColumns(cList);
            while (rs.next()) {
                ArrayList<Object> rList = new ArrayList<Object>();
                for (int i = 1; i <= cCount; i++) {
                    Object val = rs.getObject(i);
                    rList.add(val);
                }
                dInfo.addDataRow(rList);
            }
        } catch (SQLException e) {
            throw e;
        } finally {
            if (stmt != null) {
                stmt.close();
            }
            if (rs != null) {
                rs.close();
            }
        }
        return dInfo;
    }
    
    private DataAccessTabularInfo executeRelationOutput(Operator opr , Connection con) throws Exception {
        
        PreparedStatement stmt = null;
        ResultSet rs = null;
        DataAccessTabularInfo tableInfo = new DataAccessTabularInfo();
        try {
            stmt = opr.getPreparedStatmentForDebugResult(con);
            if (stmt != null) {
                rs = stmt.executeQuery();
                ResultSetMetaData md = rs.getMetaData();
                int cCount = md.getColumnCount();
                ArrayList<String> cList = new ArrayList<String>();
                for (int i = 1; i <= cCount; i++) {
                    cList.add(md.getColumnName(i));
                }
                tableInfo.setColumns(cList);
                while (rs.next()) {
                    ArrayList<Object> rList = new ArrayList<Object>();
                    for (int i = 1; i <= cCount; i++) {
                        Object val = rs.getObject(i);
                        rList.add(val);
                    }
                    tableInfo.addDataRow(rList);
                }
            } else {
                return null;
            }
        } catch (Exception e) {
            throw e;
        } finally {
            if (stmt != null) {
                stmt.close();
            }
            if (rs != null) {
                rs.close();
            }
        }
        
        return tableInfo;
    }
}
