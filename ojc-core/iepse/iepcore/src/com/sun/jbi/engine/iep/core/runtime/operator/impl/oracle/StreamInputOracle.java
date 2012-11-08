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
 * @(#)StreamInputOracle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.iep.core.runtime.operator.impl.oracle;

import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.StreamInput;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.StreamInputDb;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Connection;
import java.sql.PreparedStatement;

/**
 *
 * @author Bing Lu
 */
public class StreamInputOracle implements StreamInputDb {
    private OracleSpecial mOracleSpecial;
    
    public StreamInputOracle(OracleSpecial oracleSpecial) {
        mOracleSpecial = oracleSpecial;
    }
    
    public PreparedStatement createInsertStatement(Connection con, StreamInput op,boolean includeDBCommandForTS) throws Exception {
        String planId = op.getPlan().getId();
        String seqName = Util.getSequenceName(planId,  op.getId());
        String tableName = op.getQueueName();
        Schema schema = op.getOutputSchema();
        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(tableName);
        sb.append(" VALUES(");
        for (int i = 0, I = schema.getColumnCount(); i < I; i++) {
            sb.append("?,");
        }
        sb.append(seqName);
        sb.append(".NextVal,");
        if(includeDBCommandForTS){
        sb.append("CURRENT_TIMESTAMP");
        }else {
            sb.append("?");
        }
        sb.append(")");
        String sqlStr = sb.toString();
        PreparedStatement stmt = con.prepareStatement(sqlStr);
        return stmt;
    }
}
