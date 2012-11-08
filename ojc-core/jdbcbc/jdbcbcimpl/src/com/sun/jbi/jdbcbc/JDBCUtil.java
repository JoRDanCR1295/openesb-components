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
 * @(#)JDBCUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import java.math.BigDecimal;
import java.sql.Date;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.logging.Level;
import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;
import java.util.Arrays;
import java.util.List;


/**
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class JDBCUtil {
    
    private static final Messages mMessages = Messages.getMessages(JDBCUtil.class);
    private static final Logger mLogger = Messages.getLogger(JDBCUtil.class);
    public static final String[] dbOperations = {"insert","INSERT","update","UPDATE","delete","DELETE","select","SELECT","create","CREATE",
    											 "find","FIND","poll","POLL","drop","DROP","truncate","TRUNCATE","execute","EXECUTE"};
    public static final List<String> opList = Arrays.asList(dbOperations);
    
    /**
     *
     * @param value
     * @param jdbcType
     * @return
     * @throws Exception
     */
    static Object convert(final String value, final int jdbcType)
        throws Exception {
        Object convertedVal = null;

        try {
            switch (jdbcType) {
            case Types.BIGINT:
                convertedVal = Long.valueOf(value);

                break;

            case Types.BIT:
                convertedVal = Boolean.valueOf(value);

                break;

            case Types.BOOLEAN:
                convertedVal = Boolean.valueOf(value);

                break;

            case Types.DATE:
                convertedVal = Date.valueOf(value);

                break;

            case Types.DISTINCT:
                convertedVal = Date.valueOf(value);

                break;

            case Types.DOUBLE:
                convertedVal = Double.valueOf(value);

                break;

            case Types.DECIMAL:
            case Types.NUMERIC:
                convertedVal = new BigDecimal(value);

                break;

            case Types.FLOAT:
                convertedVal = Float.valueOf(value);

                break;

            case Types.INTEGER:
                convertedVal = Integer.valueOf(Double.valueOf(value).intValue());

                break;

            case Types.REAL:
                convertedVal = Float.valueOf(value);

                break;

            case Types.SMALLINT:
                convertedVal = Short.valueOf(value);

                break;

            case Types.TIMESTAMP:
                convertedVal = Timestamp.valueOf(value);

                break;

            case Types.BINARY:
            case Types.VARBINARY:
            case Types.LONGVARBINARY:
                convertedVal = value.getBytes();

                break;

            case Types.CHAR:
            case Types.VARCHAR:
            case Types.LONGVARCHAR:default:
                convertedVal = value;

                break;
            }
        } catch (final Exception ex) {
            JDBCUtil.mLogger.log(Level.INFO, "JDBCUtil_Failed_Convert",
                new Object[] { value, jdbcType });
            throw new Exception("Failed to convert value " + value +
                " to jdbc type " + jdbcType);
        }

        return convertedVal;
    }
    
    public static final String getSQLStatementType(final String sqlText) {
            if(opList.contains(sqlText.split("\\s")[0]))
                return sqlText.split("\\s")[0];
            else return dbOperations[dbOperations.length-1];
            /*String opName = JDBCOperations.getOpType(sqlText);
            JDBCOperations jdbcOps = JDBCOperations.getJDBCOperations(opName);
            return jdbcOps.toString();*/
    }
}
