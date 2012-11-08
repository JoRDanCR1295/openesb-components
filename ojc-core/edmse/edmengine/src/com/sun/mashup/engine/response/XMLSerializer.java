/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.mashup.engine.response;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.sql.framework.utils.XmlUtil;
/**
 *
 * @author admin
 */
public class XMLSerializer implements Serializer {

    public String serialize(ResultSet rs,String column,int offset) {
        StringBuffer rsStrBuffer = new StringBuffer();
         boolean columnFound = false;
         

        try {
            ResultSetMetaData rsmd = rs.getMetaData();          
            //int recordcount = 0;
            int recordcount= offset;
            while (rs.next()) {               
                rsStrBuffer.append("<RECORD ID =\"" + ++recordcount +"\">" );
                StringBuffer metaStrBuffer = new StringBuffer();  
                for (int i = 0; i < rsmd.getColumnCount(); i++) {
                    String colname = rsmd.getColumnName(i + 1);
                   
                    if (colname.equalsIgnoreCase(column)) {
                       
                       // rsStrBuffer = null;                                 
                       metaStrBuffer.delete(0, metaStrBuffer.length());                       
                        metaStrBuffer.append("<" + colname + ">" + XmlUtil.escapeXML(rs.getString(colname)) + "</" +colname + ">\n");                       
                        columnFound = true;
                    } else if ((!colname.equalsIgnoreCase(column) || column != null) && columnFound == false) {
                        metaStrBuffer.append("<" + colname + ">" + XmlUtil.escapeXML(rs.getString(colname)) + "</" + colname + ">\n");                   

                    }
                }                 
                 rsStrBuffer.append(metaStrBuffer);
                 rsStrBuffer.append("</RECORD>\n");
            }
        } catch (SQLException ex) {
            Logger.getLogger("global").log(Level.SEVERE, null, ex);
        }
        return rsStrBuffer.toString();
    }
}
