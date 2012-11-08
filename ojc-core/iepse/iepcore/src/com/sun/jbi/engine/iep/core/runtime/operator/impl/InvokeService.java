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
 * @(#)RelationOutput.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.operator.impl;

import com.sun.jbi.engine.iep.core.runtime.operator.Invoker;
import com.sun.jbi.engine.iep.core.runtime.operator.Request;
import com.sun.jbi.engine.iep.core.runtime.operator.Response;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.WebServiceContext;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.util.List;
import java.util.Map;
import java.sql.Connection;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.util.XmlUtil;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;
import java.sql.Clob;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.logging.Level;

import javax.xml.namespace.QName;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * RelationOutput.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class InvokeService extends AbstractOperator implements Invoker {

    private static final Messages mMessages = Messages.getMessages(InvokeService.class);
    private String[] mRequestAttrNames;
    private String[] mRequestAttrTypes;
    private int[] mRetainAttrIndex;
    private String[] mResponseAttrNames;
    private String[] mResponseAttrTypes;
    private int mOutputRowSize;  // outptutSchema's column count + 1 for timestamp
    private String mFullNamePrefix;

    public InvokeService(Map prop) {
        initialize(prop);
        try {
            Schema inputSchema = mInputOperatorList.get(0).getOutputSchema();

            mRequestAttrNames = inputSchema.getColumnNames();
            mRequestAttrTypes = inputSchema.getColumnTypes();

            List<String> retainAttrNameList = getStrList((String) prop.get(PROP_RETAIN_ATTRIBUTE_LIST));
            mRetainAttrIndex = new int[retainAttrNameList.size()];
            for (int i = 0; i < mRetainAttrIndex.length; i++) {
                String retainAttrName = retainAttrNameList.get(i);
                for (int j = 0; j < mRequestAttrNames.length; j++) {
                    if (retainAttrName.equals(mRequestAttrNames[j])) {
                        mRetainAttrIndex[i] = j;
                    }
                }
            }

            mResponseAttrNames = getStrList((String) prop.get(PROP_RESPONSE_ATTRIBUTE_LIST)).toArray(new String[0]);
            mResponseAttrTypes = new String[mResponseAttrNames.length];
            Schema outputSchema = getOutputSchema();
            String[] outputAttrNames = outputSchema.getColumnNames();
            String[] outputAttrTypes = outputSchema.getColumnTypes();
            for (int i = 0; i < mResponseAttrNames.length; i++) {
                for (int j = 0; j < outputAttrNames.length; j++) {
                    if (outputAttrNames[j].equals(mResponseAttrNames[i])) {
                        mResponseAttrTypes[i] = outputAttrTypes[j];
                    }
                }
            }

            mOutputRowSize = outputSchema.getColumnCount() + 1; // +1 for Timestamp
            mFullNamePrefix = getPlan().getName() + "." + getName() + ".";
        } catch (Exception e) {
        }
    }

    public String getOutputType() {
        return IO_TYPE_STREAM;
    }

    @Override
    protected void createOperateStmt(Connection con) throws Exception {
        if (con == null) {
            return;
        }
        mOperateStmt = mDbSpecial.getInvokeServiceDb().createOperateStatements(con, this);
    }

    @Override
    protected void executeOperateStmt(Timestamp prevT, Timestamp curT) throws Exception {
        List<Request> requestList = new ArrayList<Request>();
        ResultSet rs = null;
        try {
            int colCnt = mRequestAttrNames.length;
            mOperateStmt[0].setTimestamp(1, prevT);
            mOperateStmt[0].setTimestamp(2, curT);
            rs = mOperateStmt[0].executeQuery();
            while (rs.next()) {
                Object[] row = new Object[colCnt];
                // data
                for (int i = 0; i < colCnt; i++) {
                    row[i] = rs.getObject(i + 1);
                }
                // seqid
                String seqId = XmlUtil.getText(rs.getObject(colCnt + 1), SQL_TYPE_BIGINT);
                // timestamp
                Timestamp timestamp = rs.getTimestamp(colCnt + 2);
                Request request = new Request(seqId, row, timestamp, this);
                requestList.add(request);
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Notifier.Fail_to_retrieve_data_from_table", getQueueName(), e);
        } finally {
            Util.close(rs);
        }

        List<Response> responseList = WebServiceContext.getInstance().invoke(requestList);

        try {
            mOperateStmt[1].clearBatch();
            for (int i = 0, I = responseList.size(); i < I; i++) {
                Response response = responseList.get(i);
                for (int j = 0, J = response.getSize(); j < J; j++) {
                    Object[] row = (Object[]) response.getRow(j);
                    for (int k = 0; k < row.length; k++) {
                        mOperateStmt[1].setObject(k + 1, row[k]);
                    }
                    mOperateStmt[1].addBatch();
                }
            }
            mOperateStmt[1].executeBatch();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "InvokeService.Fail_to_insert_data_into_table", getQueueName(), e);
        }
    }

    // Invoker
    public Document getDoc(Request request) {
        Document doc = null;
        String id = request.getId();
        Object[] row = request.getRow();
        try {
            boolean namespaceAware = true;
            doc = XmlUtil.createDocument(namespaceAware);
            String namespace = getPlan().getInstanceId();
            String opName = getName();
            String typeName = opName + "_Request";
            String objName = opName + "_RequestObj";

            QName type = new QName(namespace, typeName);
            Element jbiMsgWrapper = WrapperUtil.createJBIMessageWrapper(doc, type, "output");
            doc.appendChild(jbiMsgWrapper);

            Element part = doc.createElementNS(namespace, "msgns:" + objName);
            Element jbiPartWrapper = WrapperUtil.createJBIWrappedPart(doc, part);
            jbiMsgWrapper.appendChild(jbiPartWrapper);

            Element r = null;
            for (int i = 0; i < mRequestAttrNames.length; i++) {
                r = doc.createElementNS(null, mRequestAttrNames[i]);
                part.appendChild(r);
                String attrType = mRequestAttrTypes[i];
                String value = null;
                if (SQL_TYPE_CLOB.equals(attrType)) {
                    Clob clob = (Clob) row[i];
                    value = clob.getSubString((long) 1, (int) clob.length());
                } else if (SQL_TYPE_DATE.equals(attrType)) {
                    value = XmlUtil.getText((Date) row[i], SQL_TYPE_DATE);
                } else if (SQL_TYPE_TIME.equals(attrType)) {
                    value = XmlUtil.getText((Time) row[i], SQL_TYPE_TIME);
                } else if (SQL_TYPE_TIMESTAMP.equals(attrType)) {
                    value = XmlUtil.getText((Timestamp) row[i], SQL_TYPE_TIMESTAMP);
                } else if (SQL_TYPE_BIGINT.equals(attrType)) {
                    value = XmlUtil.getText((Long) row[i], attrType);
                } else if (SQL_TYPE_INTEGER.equals(attrType)) {
                    value = XmlUtil.getText((Integer) row[i], attrType);
                } else if (SQL_TYPE_DOUBLE.equals(attrType)) {
                    value = XmlUtil.getText((Double) row[i], attrType);
                } else {
                    value = XmlUtil.getText(row[i], attrType);
                }
                XmlUtil.constructColumnNodeContent(doc, r, mRequestAttrNames[i], mRequestAttrTypes[i], value);
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE,
                    mMessages.getString("Notifier.Fail_to_create_xml_message_for_row", mFullNamePrefix + "." + id),
                    e);
        }
        return doc;
    }

    public List<Object[]> getRowList(Response response) {
        List<Object[]> rowList = new ArrayList<Object[]>();
        try {
            Element rootNode = response.getXml();
            List<Object[]> responseRowList = XmlUtil.responseToList(getName(), rootNode, mResponseAttrNames, mResponseAttrTypes);
            for (int i = 0; i < responseRowList.size(); i++) {
                Object[] row = new Object[mOutputRowSize];
                // assign retain attributes
                Object[] requestRow = response.getRequest().getRow();
                for (int j = 0; j < mRetainAttrIndex.length; j++) {
                    row[j] = requestRow[mRetainAttrIndex[j]];
                }
                // assign response attributes
                Object[] responseRow = responseRowList.get(i);
                System.arraycopy(responseRow, 0, row, mRetainAttrIndex.length, responseRow.length);
                // assign timestamp
                row[mOutputRowSize-1] = response.getRequest().getTimestamp();
                
                rowList.add(row);
            }
        } catch (Exception e) {
        }
        return rowList;
    }
}
