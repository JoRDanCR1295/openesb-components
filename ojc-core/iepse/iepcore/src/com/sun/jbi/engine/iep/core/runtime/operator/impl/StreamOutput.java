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
 * @(#)StreamOutput.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import java.util.Map;
import java.sql.Connection;
import java.sql.PreparedStatement;
import com.sun.jbi.engine.iep.core.runtime.operator.ColumnMetadata;
import com.sun.jbi.engine.iep.core.runtime.operator.Notification;
import com.sun.jbi.engine.iep.core.runtime.operator.Notifier;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.util.PropertyUtil;
import com.sun.jbi.engine.iep.core.runtime.util.XmlUtil;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;
import java.sql.Clob;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;
import javax.xml.namespace.QName;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;

/**
 * StreamOutput.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class StreamOutput extends AbstractOperator implements Notifier {
    private static final Messages mMessages = Messages.getMessages(StreamOutput.class);

    private int mEventType = EVENT_TYPE_XML_DOCUMENT;
    private boolean mIncludeTimestamp;
    private String mGetBatchStr;
    private String mDeleteStr;
    private String mDeleteBatchStr;
    private List<Object> mEventList;
    private List<String> mSeqIdList;
    private String mFullNamePrefix;
    private Timestamp mLastFetchTime = new Timestamp((new Date(0L)).getTime());

    public StreamOutput(Map prop) {
        initialize(prop);
        mIncludeTimestamp = PropertyUtil.getboolean(prop, PROP_INCLUDE_TIMESTAMP, false);
        String queueName = getQueueName();
        StringBuffer sb = new StringBuffer();
        sb.append("SELECT * FROM ");
        sb.append(queueName);
        sb.append(" WHERE ? < " + COL_TIMESTAMP + " AND " +  COL_TIMESTAMP + " <= ?  ORDER BY " + COL_SEQID);
        mGetBatchStr = sb.toString();

        sb = new StringBuffer();
        sb.append("DELETE FROM ");
        sb.append(queueName);
        sb.append(" WHERE " + COL_SEQID + " = ?");
        mDeleteStr = sb.toString();
        
        sb = new StringBuffer();
        sb.append("DELETE FROM ");
        sb.append(queueName);
        sb.append(" WHERE " + COL_SEQID + " <= ?");
        mDeleteBatchStr = sb.toString();

        mEventList = new ArrayList<Object>();
        mSeqIdList = new ArrayList<String>();
        
        mFullNamePrefix =  getPlan().getName() + "." + getName() + ".";
    }

    public String getOutputType() {
        return IO_TYPE_NONE;
    }

    // template method: used by deploy()
    @Override
    protected void createOutputQueue(Connection con) throws Exception {
        Schema schema = getOutputSchema();
        String tableName = getQueueName();
        mDbSpecial.createStream(con, tableName, schema, new ArrayList<ColumnMetadata>(), false);
    }

    // template method: used by undeploy()
    @Override
    protected void dropOutputQueue(Connection con) throws Exception {
        String tableName = getQueueName();
        Util.dropTable(con, tableName);
    }

    // template method: used by setConnection(..)
    @Override
    protected void createCleanOutputStmt(Connection con) throws Exception {
        // It is the user's responsibility to clean the table
        mCleanOutputStmt = null;
    }

    @Override
    protected void createOperateStmt(Connection con) throws Exception {
        if (con == null) {
            return;
        }

        String outputTableName = getQueueName();
        String inputTableName = mInputOperatorList.get(0).getQueueName();
        // Prepare mOperateStmt:
        // INSERT INTO S2  
        //    SELECT * FROM S1
        //       WHERE 7 < t1.Timestamp AND t1.Timestamp <= 10
        StringBuffer sb = new StringBuffer();
        sb.append("INSERT INTO ");
        sb.append(outputTableName);
        sb.append(" SELECT * FROM ");
        sb.append(inputTableName);
        sb.append(" WHERE ? < " + COL_TIMESTAMP + " AND " + COL_TIMESTAMP + " <= ? ");
        String sqlStr = sb.toString();
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Operator.operateStmt", new Object[]{getName(), sqlStr});
        }
        mOperateStmt = new PreparedStatement[]{con.prepareStatement(sqlStr)};
    }

    // Notifier
    public void setEventType(int eventType) {
        mEventType = eventType;
    }

    private Document buildMessage(String[] row, String seqId) {
        Document doc = null;
        try {
            boolean namespaceAware = true;
            doc = XmlUtil.createDocument(namespaceAware);
            String namespace = getPlan().getInstanceId();
            String opName = getName();
            String typeName = opName + "_Msg";
            String objName = opName + "_MsgObj";

            QName type = new QName(namespace, typeName);
            Element jbiMsgWrapper = WrapperUtil.createJBIMessageWrapper(doc, type, "output");
            doc.appendChild(jbiMsgWrapper);

//            Element part = doc.createElementNS(namespace, "msgns1:" + objName);
            Element part = doc.createElementNS(namespace, "msgns:" + objName);
            Element jbiPartWrapper = WrapperUtil.createJBIWrappedPart(doc, part);
            jbiMsgWrapper.appendChild(jbiPartWrapper);

            String[] columnNames = getOutputSchema().getColumnNames();
            String[] columnTypes = getOutputSchema().getColumnTypes();
            Element r = null;
            Text t = null;
            for (int j = 0; j < columnNames.length; j++) {
                r = doc.createElementNS(null, columnNames[j]);
                part.appendChild(r);

                String columnType = columnTypes[j];
                XmlUtil.constructColumnNodeContent(doc, r, columnNames[j], columnType, row[j]);

            }
            if (mIncludeTimestamp) {
                r = doc.createElementNS(null, "Timestamp");
                part.appendChild(r);
                t = doc.createTextNode(row[row.length - 1].toString());
                r.appendChild(t);
            }

        } catch (Exception e) {
            mMessages.log(Level.SEVERE,
                    mMessages.getString("Notifier.Fail_to_create_xml_message_for_row", mFullNamePrefix + "." + seqId),
                    e);
        }
        return doc;
    }
    
    public boolean includeTimestamp() {
        return mIncludeTimestamp;
    }
    
    public int getCacheSize() {
        return mSeqIdList.size();
    }

    public void fetch(Connection con, Timestamp timestamp) {
        if (!mLastFetchTime.before(timestamp)) {
            return;
        } 
        ResultSet rs = null;
        PreparedStatement getStmt = null;
        try {
            Schema schema = getOutputSchema();
            int colCnt = schema.getColumnCount();  // data
            String[] colTypes = schema.getColumnTypes();
            getStmt = con.prepareStatement(mGetBatchStr);

            getStmt.setTimestamp(1, mLastFetchTime);
            getStmt.setTimestamp(2, timestamp);
            rs = getStmt.executeQuery();
            switch (mEventType) {
                case EVENT_TYPE_OBJECT_ARRAY:
                    while (rs.next()) {
                        Object[] row = new Object[colCnt + 1]; // data, timestamp
                        // data
                        for (int i = 0; i < colCnt; i++) {
                            String type = colTypes[i];
                            if (SQL_TYPE_CLOB.equals(type)) {
                                Clob clob = rs.getClob(i + 1);
                                row[i] = clob.getSubString((long) 1, (int) clob.length());
                            } else if (SQL_TYPE_DATE.equals(type)) {
                                row[i] = rs.getDate(i + 1);
                            } else if (SQL_TYPE_TIME.equals(type)) {
                                row[i] = rs.getTime(i + 1);
                            } else if (SQL_TYPE_TIMESTAMP.equals(type)) {
                                row[i] = rs.getTimestamp(i + 1);
                            } else if (SQL_TYPE_BIGINT.equals(type)) {
                                row[i] = rs.getLong(i + 1);
                            } else if (SQL_TYPE_INTEGER.equals(type)) {
                                row[i] = rs.getInt(i + 1);
                            } else if (SQL_TYPE_DOUBLE.equals(type)) {
                                row[i] = rs.getDouble(i + 1);
                            } else {
                                row[i] = rs.getObject(i + 1);
                            }
                        }

                        //seqid
                        String seqId = XmlUtil.getText(rs.getObject(colCnt + 1), SQL_TYPE_BIGINT);
                        // timestamp
                        row[colCnt] = rs.getTimestamp(colCnt + 2);
                        mEventList.add(row);
                        mSeqIdList.add(seqId);
                    }
                    break;
                case EVENT_TYPE_MAP:
                    String[] colNames = schema.getColumnNames();
                    while (rs.next()) {
                        Map<String, Object> row = new HashMap<String, Object>(); // data, timestamp
                        // data
                        for (int i = 0; i < colCnt; i++) {
                            String type = colTypes[i];
                            if (SQL_TYPE_CLOB.equals(type)) {
                                Clob clob = rs.getClob(i + 1);
                                row.put(colNames[i], clob.getSubString((long) 1, (int) clob.length()));
                            } else if (SQL_TYPE_DATE.equals(type)) {
                                row.put(colNames[i], rs.getDate(i + 1));
                            } else if (SQL_TYPE_TIME.equals(type)) {
                                row.put(colNames[i], rs.getTime(i + 1));
                            } else if (SQL_TYPE_TIMESTAMP.equals(type)) {
                                row.put(colNames[i], rs.getTimestamp(i + 1));
                            } else if (SQL_TYPE_BIGINT.equals(type)) {
                                row.put(colNames[i], rs.getLong(i + 1));
                            } else if (SQL_TYPE_INTEGER.equals(type)) {
                                row.put(colNames[i], rs.getInt(i + 1));
                            } else if (SQL_TYPE_DOUBLE.equals(type)) {
                                row.put(colNames[i], rs.getDouble(i + 1));
                            } else {
                                row.put(colNames[i], rs.getObject(i + 1));
                            }
                        }

                        //seqid
                        String seqId = XmlUtil.getText(rs.getObject(colCnt + 1), SQL_TYPE_BIGINT);
                        // timestamp
                        row.put(COL_TIMESTAMP, rs.getTimestamp(colCnt + 2));
                        mEventList.add(row);
                        mSeqIdList.add(seqId);
                    }
                    break;
                case EVENT_TYPE_XML_DOCUMENT:
                    while (rs.next()) {
                        String[] row = new String[colCnt + 1]; // data, timestamp
                        // data
                        for (int i = 0; i < colCnt; i++) {
                            String type = colTypes[i];
                            if (SQL_TYPE_CLOB.equals(type)) {
                                Clob clob = rs.getClob(i + 1);
                                row[i] = clob.getSubString((long) 1, (int) clob.length());
                            } else if (SQL_TYPE_DATE.equals(type)) {
                                row[i] = XmlUtil.getText(rs.getDate(i + 1), SQL_TYPE_DATE);
                            } else if (SQL_TYPE_TIME.equals(type)) {
                                row[i] = XmlUtil.getText(rs.getTime(i + 1), SQL_TYPE_TIME);
                            } else if (SQL_TYPE_TIMESTAMP.equals(type)) {
                                row[i] = XmlUtil.getText(rs.getTimestamp(i + 1), SQL_TYPE_TIMESTAMP);
                            } else if (SQL_TYPE_BIGINT.equals(type)) {
                                row[i] = XmlUtil.getText(rs.getLong(i + 1), type);
                            } else if (SQL_TYPE_INTEGER.equals(type)) {
                                row[i] = XmlUtil.getText(rs.getInt(i + 1), type);
                            } else if (SQL_TYPE_DOUBLE.equals(type)) {
                                row[i] = XmlUtil.getText(rs.getDouble(i + 1), type);
                            } else {
                                row[i] = XmlUtil.getText(rs.getObject(i + 1), type);
                            }
                        }

                        //seqid
                        String seqId = XmlUtil.getText(rs.getLong(colCnt + 1), SQL_TYPE_BIGINT);
                        // timestamp
                        row[colCnt] = XmlUtil.getText(rs.getTimestamp(colCnt + 2), SQL_TYPE_TIMESTAMP);
                        mEventList.add(buildMessage(row, seqId));
                        mSeqIdList.add(seqId);
                    }
                    break;
            }

            mLastFetchTime = timestamp;
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Notifier.Fail_to_retrieve_data_from_table", getQueueName(), e);
        } finally {
            Util.close(rs);
            Util.close(getStmt);
        }        
    }
    
    
    public Notification getNotification() {
        if (mSeqIdList.size() > 0) {
            String id = mFullNamePrefix + mSeqIdList.get(0);
            Notification n = new Notification(id, mEventList.get(0), mSeqIdList.get(0));
            return n;
        }
        return null;
    }
    
    public List<Notification> getNotificationBatch(int maxBatchSize) {
        List<Notification> ret = new ArrayList<Notification>();
        if (mSeqIdList.size() > 0) {
            int I = Math.min(mSeqIdList.size(), maxBatchSize);
            for (int i = 0; i < I; i++) {
                String id = mFullNamePrefix + mSeqIdList.get(i);
                Notification n = new Notification(id, mEventList.get(i), mSeqIdList.get(i));
                ret.add(n);
            }    
        }
        return ret;
    }

    public void removeNotification(Connection con, Notification n) throws Exception {
        String seqId = n.getSeqId(0);
        PreparedStatement deleteStmt = null;
        try {
            int idx = mSeqIdList.indexOf(seqId);
            if (idx >= 0) {
                mEventList.remove(idx);
                mSeqIdList.remove(idx);
            }
            deleteStmt = con.prepareStatement(mDeleteStr);
            deleteStmt.setObject(1, seqId);
            deleteStmt.executeUpdate();
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Notifier.Fail_to_delete_row", 
            		getQueueName() + "." + seqId), e);            
        } finally {
            Util.close(deleteStmt);
        }
    }

    public void removeNotificationBatch(Connection con, List<Notification> batch) throws Exception {
        long maxSeqId = 0;
        for (Notification n : batch) {
            long seqId = Long.parseLong(n.getSeqId(0));
            maxSeqId = Math.max(seqId, maxSeqId);
        }
        PreparedStatement deleteBatchStmt = null;
        try {
            int idx = mSeqIdList.indexOf("" + maxSeqId);
            for (int i = idx; i >= 0; i--) {
                mEventList.remove(i);
                mSeqIdList.remove(i);
            }
            deleteBatchStmt = con.prepareStatement(mDeleteBatchStr);
            deleteBatchStmt.setObject(1, maxSeqId);
            deleteBatchStmt.executeUpdate();
        } catch (Exception e) {
        	throw new Exception(mMessages.getString("Notifier.Fail_to_delete_rows_at_and_before", 
        			getQueueName() + "." + maxSeqId), e);
        } finally {
            Util.close(deleteBatchStmt);
        }
    }
}
