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
 * @(#)Util.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.iep.core.derby;

import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;


/*
 * Util.java
 *
 * Created on August 27, 2005, 10:04 AM
 *
 * @author Bing Lu
 *
 * NOTE THAT: If you change this class, you need to copy iepderby.jar into derby's server classpath.
 * For example: C:\Alaska\root\jbi\runtime\Sun\AppServer\derby\lib
 */

public class Util implements OperatorConstants {
    public static String getTableUsageTableName(String planId) {
        return TABLE_EMS_TABLE_USAGE + "_" + planId;
    }
    
    public static List<String> getTokenList(String s, String delim) {
        StringTokenizer st = new StringTokenizer(s, delim);
        ArrayList<String> list = new ArrayList<String>();
        while (st.hasMoreTokens()) {
            String t = st.nextToken();
            if (t.trim().equals("")) {
                continue;
            }
            list.add(t);
        }
        return list;
    }
    
    public static String[] getTokens(String s, String delim) {
        return (String[])getTokenList(s, delim).toArray(new String[0]);
    }
    
    public static String[] getColumnExpressions(String s) {
        List<String> list = getTokenList(s, DELIM);
        String[] ret = new String[list.size()];
        for (int i = 0; i < ret.length; i++) {
            String colName = list.get(i);
            if (colName.startsWith(SINGLE_QUOTE_SUB) && colName.endsWith(SINGLE_QUOTE_SUB)) {
                int len = colName.length();
                ret[i] = "'" + colName.substring(1, len-1) + "'";
            } else {
                ret[i] = colName;
            }
        }
        return ret;
    }
    
    public static void close(Statement stmt) {
        try {
            if (stmt != null) {
                stmt.close();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    public static void close(Connection con) {
        try {
            if (con != null) {
                con.close();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    public static void close(ResultSet rs) {
        try {
            if (rs != null) {
                rs.close();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
