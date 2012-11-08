/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.mashup.engine.response;

import com.sun.rowset.WebRowSetImpl;
import java.io.StringWriter;
import java.sql.ResultSet;
import javax.sql.rowset.WebRowSet;

/**
 *
 * @author admin
 */
public class WebRowSetSerializer implements Serializer {
    
    public String serialize(ResultSet rs,String column,int offset) throws Exception {
        StringBuffer sb = new StringBuffer();
        WebRowSet wrs = new WebRowSetImpl();
        wrs.populate(rs);
        // TODO Relook this piece of code.       
        StringWriter writer = new StringWriter();
        wrs.writeXml(writer);
        String content = writer.toString();
        content = content.substring(content.indexOf("?>") + 2);
        sb.append(content);
        return sb.toString();
    }
}
