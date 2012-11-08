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
 * @(#)TableInput.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import java.sql.Timestamp;
import java.sql.Connection;
import java.util.Map;

import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.util.HashSet;

/**
 * TableInput.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class TableInput extends AbstractOperator {
    
    private boolean mDoNotCreateTable;
    
    public TableInput(Map prop) {
        initialize(prop);
        
        mDoNotCreateTable = PropertyUtil.getboolean(prop, PROP_DO_NOT_CREATE_TABLE, false);
    }

    public String getOutputType() {
        return IO_TYPE_TABLE;
    }

    @Override
    protected void createOutputQueue(Connection con) throws Exception {
        //if mDoNotCreateTable is set to true we will do not create/delete
        //table. we assume a table should exists 
        if(mDoNotCreateTable) {
            return;
        }
        
        Schema schema = getOutputSchema();
        String tableName = getQueueName();
        int status = mDbSpecial.checkTableStatus(con, mDbSchemaName, tableName, schema, new HashSet());
        switch (status) {
            case TS_UNKNOWN:
                Util.dropTable(con, tableName);
                mDbSpecial.createTable(con, tableName, schema);
                break;
            case TS_NAME_NOT_EXIST:
                mDbSpecial.createTable(con, tableName, schema);
                break;
            case TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA:
                Util.dropTable(con, tableName);
                mDbSpecial.createTable(con, tableName, schema);
                break;
        }                            
    }
    
    @Override
    protected void dropOutputQueue(Connection con) throws Exception {
    }

    @Override
    public boolean hasWorkToDo(Timestamp timestampToProcess) {
        return false;
    }
    
    @Override
    public void operate(Timestamp timestampToProcess) {
    }
    
}    
        
