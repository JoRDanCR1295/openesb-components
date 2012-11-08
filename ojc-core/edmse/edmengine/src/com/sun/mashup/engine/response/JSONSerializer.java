/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.mashup.engine.response;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.sql.framework.utils.XmlUtil;
/**
 *
 * @author admin
 */
public class JSONSerializer implements Serializer {

    public String serialize(ResultSet rs,String column,int offset) {
        ResultSetMetaData rsmd = null;
        StringBuffer rsStrBuffer = new StringBuffer();
        String colname = null;
        String productName = null;
        String supplierAddress = null;
        List supplierAddrList = null;
        int colwidth;
        int numberOfColumns = 0;
        String prefix1 = "{columns :[";
        String suffix1 = "]}";
        String responseStr = null;

        try {
            boolean columnFound = false; 
            StringBuffer metaStrBuffer = new StringBuffer();
            rsmd = rs.getMetaData();
            numberOfColumns = rsmd.getColumnCount();
            for (int i = 1; i <= numberOfColumns; i++) {
                colname = rsmd.getColumnName(i);
                // colwidth = rsmd.getColumnDisplaySize(i);
                if (colname.equalsIgnoreCase(column)) {
                    metaStrBuffer.delete(0, metaStrBuffer.length());
                    metaStrBuffer.append("{").append("id:").append("'").append(colname).append("'");
                    metaStrBuffer.append(",");
                    metaStrBuffer.append("label:'").append(colname).append("'");
                    metaStrBuffer.append("}");
                    columnFound = true;
                } else if ((!colname.equalsIgnoreCase(column) || column != null) && columnFound == false) {

                    metaStrBuffer.append("{").append("id:").append("'").append(colname).append("'");
                    metaStrBuffer.append(",");
                    metaStrBuffer.append("label:'").append(colname).append("'");
                    metaStrBuffer.append("}");

                    if (i != numberOfColumns) {
                        metaStrBuffer.append(",");
                    }
                }
            }

            rsStrBuffer.append(metaStrBuffer);
            rsStrBuffer.append("],");
        } catch (SQLException ex) {
            Logger.getLogger("global").log(Level.SEVERE, null, ex);
        }
        rsStrBuffer.append("rows :[");
        
         
         boolean colFound = false;        
         String bracesString ="{";
         try {
             while (rs.next()) {
                StringBuffer rowStrBuffer = new StringBuffer();               
                //rowStrBuffer.append("{");
                for (int i = 0; i < numberOfColumns; i++) {
                    colname = rsmd.getColumnName(i + 1);
                    if (colname.equalsIgnoreCase(column)) {
                         
                        rowStrBuffer.delete(0,rowStrBuffer.length() );
                       
                        rowStrBuffer.append(colname + ":'" + XmlUtil.escapeXML(rs.getString(colname)) + "'");
                        colFound = true;
                    } else if (!colname.equalsIgnoreCase(column) && colFound == false) {
                         
                        rowStrBuffer.append(colname + ":'" +XmlUtil.escapeXML(rs.getString(colname))  + "'");
                        if (i != rsmd.getColumnCount() - 1) {
                            rowStrBuffer.append(",");
                        }
                    }
                }                
                rsStrBuffer.append(bracesString);
                rsStrBuffer.append(rowStrBuffer);                
                rsStrBuffer.append("},");
            }
            responseStr = rsStrBuffer.toString();
            responseStr = responseStr.substring(0, responseStr.lastIndexOf("},") + 1);

        } catch (SQLException ex) {
            Logger.getLogger("global").log(Level.SEVERE, null, ex);
        }
        rsStrBuffer = new StringBuffer(responseStr.length());
        rsStrBuffer.append(prefix1);
        rsStrBuffer.append(responseStr);
        rsStrBuffer.append(suffix1);
        return rsStrBuffer.toString();
    }
}
