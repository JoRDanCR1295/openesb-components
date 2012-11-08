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
 * @(#)Distinct.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import java.util.Map;
import java.sql.Timestamp;
import java.sql.Connection;
import java.util.ArrayList;

/**
 * Distinct.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */ 
public class Distinct extends AbstractOperator {
    public Distinct(Map prop) {
        initialize(prop);
    }
    
    public String getOutputType() {
        return IO_TYPE_RELATION;
    }

    // template method: used by deploy()
    @Override
    protected void createOutputQueue(Connection con) throws Exception {
        Schema schema = getOutputSchema();
        String tableName = getQueueName();
        mDbSpecial.createRelation(con, tableName, schema, new ArrayList<ColumnMetadata>(), true);
    }

    @Override
    protected void createSynopsis(Connection con) throws Exception {
        mDbSpecial.createSequence(con, this);
    }
    
    @Override
    protected void dropSynopsis(Connection con) throws Exception {
        mDbSpecial.dropSequence(con, this);
    }
    
    @Override
    protected void createOperateStmt(Connection con) throws Exception {
        if (con ==  null) {
            return;
        }
        // Prepare mOperateStmt:
        mOperateStmt = mDbSpecial.getDistinctDb().createOperateStatements(con, this);
    }

    @Override
    protected void executeOperateStmt(Timestamp prevT, Timestamp curT) throws Exception {
        int inputCount = mInputOperatorList.size();
        for (int i = 0; i < inputCount; i++) {
            mOperateStmt[0].setTimestamp(2*i+1, prevT);
            mOperateStmt[0].setTimestamp(2*i+2, curT);
        }
        mOperateStmt[0].executeUpdate();
        for (int i = 0; i < inputCount; i++) {
            mOperateStmt[1].setTimestamp(2*i+1, prevT);
            mOperateStmt[1].setTimestamp(2*i+2, curT);
        }
        mOperateStmt[1].executeUpdate();
    }

}

        
