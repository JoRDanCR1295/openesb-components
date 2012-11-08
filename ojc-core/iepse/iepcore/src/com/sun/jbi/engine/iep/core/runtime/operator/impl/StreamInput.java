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
 * @(#)StreamInput.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.Inserter;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Timestamp;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.Map;
import java.util.logging.Level;

/**
 * StreamInput.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class StreamInput extends AbstractOperator implements Inserter {
    private static final Messages mMessages = Messages.getMessages(AbstractOperator.class);

    public StreamInput(Map prop) {
        initialize(prop);
    }
    
    public String getOutputType() {
        return IO_TYPE_STREAM;
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
    public boolean hasWorkToDo(Timestamp timestampToProcess) {
        return false;
    }
    
    public void operate(Timestamp timestampToProcess) {
        long startTime = System.currentTimeMillis();
        try {
            if (mGarbageCollectionEnabled && mCleanOutputStmt != null) {
                // clean up output queue using previous timestamp
                mCleanOutputStmt.executeUpdate();
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Operator.Operator_fail_to_operate", getName(), e);
            Util.rollback(mRuntimeConnection);
        } finally {
            mProcessingTime += (System.currentTimeMillis() - startTime);
        }
    }
    
    public PreparedStatement getInsertStatement(Connection con,boolean includeDBCommandForTS) throws Exception {
        return mDbSpecial.getStreamInputDb().createInsertStatement(con, this,includeDBCommandForTS);
    }

}    
        
