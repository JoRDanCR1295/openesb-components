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
 * @(#)InsertCommand.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.client.pojo;

import com.sun.jbi.engine.iep.core.runtime.operator.Inserter;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import java.sql.Timestamp;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;

/**
 * InsertCommand.java
 *
 * Created on September 8, 2005, 1:28 AM
 *
 * @author Bing Lu
 */
public class InsertCommand implements Command {
    private static final Messages mMessages = Messages.getMessages(InsertCommand.class);
    
    private static SimpleDateFormat mSDF = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");
    private Properties mConfigProp;
    private Connection mCon = null;
    private PreparedStatement mStmt = null;
    private PreparedStatement mClearStmt = null;
    private QueryPlan mPlan = null;
    private Operator mOperator = null;

    /** Creates a new instance of InsertCommand */
    public InsertCommand() {
    }

    // props:
    //     instanceId
    //     opId
    // System.getPrroperties:
    //     DbType 
    //     RuntimeStyle
    //     DbHostname
    //     DbPort
    //     DbUsername
    //     DbPassword
    //     DbSchema
    //     DbSid
    public void init(NWaySheperd sheperd, String props) {
        try {
            mConfigProp = PropertyUtil.getPropertiesFromString(props, "=", ";");
            mConfigProp.putAll(System.getProperties());
            mCon = Util.getConnection(mConfigProp);
            String instanceId = (String) mConfigProp.get("instanceId");
            String opId = PropertyUtil.getString(mConfigProp, "opId", "");
            mPlan = Util.getPlanByInstanceId(mConfigProp, instanceId);
            mOperator = mPlan.getOperatorById(opId);
            mStmt = ((Inserter) mOperator).getInsertStatement(mCon, true);
            String outputTableName = mOperator.getQueueName();
            mClearStmt = Util.createCleanStreamByMinUsageTimeStmt(mCon, mPlan.getId(), outputTableName);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "InsertCommand.init_fails", e);
        }
    }

    public Runnable createRunnable(final Map<String, Input> inputTable, final String[] args) {
        return new Runnable() {
            public void run() {
                Input input = inputTable.get(args[1]);
                int batches = 1;
                try {
                    batches = Integer.parseInt(args[2]);
                } catch (Exception e) {
                }
                try {
                    String outputTableName = mOperator.getQueueName();
                    int colCnt = mOperator.getOutputSchema().getColumnCount();
                    Timestamp minUsageTimeStamp = Util.getTableUsage(mCon, mPlan.getId(), outputTableName);
                    if (minUsageTimeStamp != null) {
                        StringBuffer sb = new StringBuffer();
                        mClearStmt.executeUpdate();
                        for (int i = 0; i < batches; i++) {
                            if (0 < i) {
                                sb.append("\n");
                            }
                            sb.append("(");
                            String data = input.nextData();
                            String[] row = input.getCurRow();
                            for (int k = 0; k < colCnt; k++) {
                                mStmt.setObject(k + 1, row[k]);
                                if (0 < k) {
                                    sb.append(",");
                                }
                                sb.append(row[k]);
                            }
                            mStmt.addBatch();
                            sb.append(")");
                        }
                        mStmt.executeBatch();
                        if (mMessages.isLoggable(Level.FINE)) {
                            mMessages.log(Level.FINE,
                                    "InsertCommand.Insert_into_table", new Object[]{outputTableName, sb.toString(), mSDF.format(new Date())});
                        }
                    }
                } catch (Exception e) {
                    mMessages.log(Level.SEVERE, "InsertCommand.run_fails", e);
                }
            }
        };
    }

    public void destroy() {
        Util.close(mCon);
    }
}
