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
 * @(#)InputOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep;

import com.sun.jbi.engine.iep.IEPSEInOnlyThread.FaultCode;
import com.sun.jbi.engine.iep.core.runtime.operator.Inserter;
import com.sun.jbi.engine.iep.core.runtime.operator.Operator;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.engine.iep.core.runtime.util.XmlUtil;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.util.WrapperUtil;

import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.xml.transform.Source;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * InputOperation.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 * 
 */
public class InputOperation {

    private static final Messages mMessages = Messages.getMessages(InputOperation.class);
    private static final Logger mLogger = Messages.getLogger(InputOperation.class);
    private static final String FAULTCODE_PROPERTY_NAME = "com.sun.jbi.crl.faultcode";
    private static final String FAULTSTRING_PROPERTY_NAME = "com.sun.jbi.crl.faultstring";
    private static final String FAULTACTOR_PROPERTY_NAME = "com.sun.jbi.crl.faultactor";
    private static final String FAULTDETAIL_PROPERTY_NAME = "com.sun.jbi.crl.faultdetail";

    public static void sendError(DeliveryChannel channel, InOnly inOnly, String actor, FaultCode code, Exception e) {
        try {
            inOnly.setStatus(ExchangeStatus.ERROR);
            inOnly.setError(e);
            inOnly.setProperty(FAULTSTRING_PROPERTY_NAME, e.getLocalizedMessage());
            inOnly.setProperty(FAULTCODE_PROPERTY_NAME, String.valueOf(code));
            inOnly.setProperty(FAULTDETAIL_PROPERTY_NAME, e.getLocalizedMessage());
            inOnly.setProperty(FAULTACTOR_PROPERTY_NAME, actor);
            channel.send(inOnly);
        } catch (Exception ex) {
        }
    }
    private String mOperation;
    private Operator mOp;
    private ExtendedComponentContext mExtendedContext;
    private LinkedList<InOnly> mInOnlyList = new LinkedList<InOnly>();
    private LinkedList<List<Object[]>> mDataList = new LinkedList<List<Object[]>>();

    private void addBatch(InOnly inOnly, List<Object[]> rowList) {
        mInOnlyList.add(inOnly);
        mDataList.add(rowList);
    }
    
    @SuppressWarnings("unchecked")
    private void input(Connection con, List rowList) throws Exception {
        PreparedStatement insertStmt = null;
        String queueName = mOp.getQueueName();
        try {
            insertStmt = ((Inserter) mOp).getInsertStatement(con, true);
            if (rowList.size() > 1) {
                insertStmt.clearBatch();
                for (int i = 0, I = rowList.size(); i < I; i++) {
                    Object[] row = (Object[]) rowList.get(i);
                    for (int j = 0; j < row.length; j++) {
                        insertStmt.setObject(j + 1, row[j]);
                    }
                    insertStmt.addBatch();
                }
                insertStmt.executeBatch();
            } else {
                Object[] row = (Object[]) rowList.get(0);
                for (int j = 0; j < row.length; j++) {
                    insertStmt.setObject(j + 1, row[j]);
                }
                insertStmt.executeUpdate();
            }
        } catch (Exception e) {
            String err = mMessages.getString("InputOperation.Fail_to_insert_data_into_table", queueName);
            mLogger.log(java.util.logging.Level.SEVERE, err, e);
            throw new ServerException(err, e);
        } finally {
            Util.close(insertStmt);
        }
    }
    
    InputOperation(ExtendedComponentContext extendedContext, QueryPlan plan, String operation) {
        mExtendedContext = extendedContext;
        try {
            mOperation = operation;
            if (mOperation.endsWith("Batch")) {
                mOperation = mOperation.substring(0, mOperation.length() - 5);
            }
            mOp = plan.getOperatorByOperation(mOperation);
            if (mOp == null) {
                throw new Exception(mMessages.getString("InputOperation.No_operator_has_operation", mOperation));
            }
        } catch (Exception e) {
            String msg = mMessages.getString("InputOperation.Fail_to_initialize_InputOperation", new Object[]{plan.getName(), mOperation});
            mLogger.log(java.util.logging.Level.SEVERE, msg, e);
        }
    }

    public void sendError(FaultCode code, Exception e) {
        String actor = mExtendedContext.getComponentContext().getComponentName();
        DeliveryChannel channel = mExtendedContext.getDeliveryChannel();
        for (InOnly inOnly : mInOnlyList) {
            sendError(channel, inOnly, actor, code, e);
        }
    }

    public void process(InOnly inOnly) {
        DeliveryChannel channel = mExtendedContext.getDeliveryChannel();
        NormalizedMessage request = inOnly.getInMessage();
        Source src = request.getContent();
        Element objNode = null;
        try {
            Document doc = XmlUtil.createDocumentFromSource(src);
            Element rootNode = doc.getDocumentElement();
            objNode = WrapperUtil.getPartElement(doc);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("InputOperation.Inbound_message",
                        XmlUtil.toXml(rootNode, "UTF-8", false)));
            }
        } catch (Exception e) {
            String actor = mExtendedContext.getComponentContext().getComponentName();
            String msg = mMessages.getString("InputOperation.Bad_content", XmlUtil.toXml(objNode, "UTF-8", false));
            sendError(channel, inOnly, actor, FaultCode.Client, e);
            return;
        }
        if (objNode == null || !objNode.getTagName().endsWith("_MsgObj")) {
            String actor = mExtendedContext.getComponentContext().getComponentName();
            String msg = mMessages.getString("InputOperation.Bad_content", XmlUtil.toXml(objNode, "UTF-8", false));
            Exception e = new ClientException(msg);
            sendError(channel, inOnly, actor, FaultCode.Client, e);
            return;
        }
        Schema schema = mOp.getOutputSchema();
        List<Object[]> rowList;
        try {
            rowList = XmlUtil.messageToList(mOperation, objNode, schema);
        } catch (Exception e) {
            String actor = mExtendedContext.getComponentContext().getComponentName();
            String msg = mMessages.getString("InputOperation.Bad_content", XmlUtil.toXml(objNode, "UTF-8", false));
            Exception ce = new ClientException(msg);
            sendError(channel, inOnly, actor, FaultCode.Client, ce);
            return;
        }
        boolean isXa = inOnly.isTransacted();
        Connection con = null;
        if (isXa) {
            // When isXa is true, IEPSE participates in the inbound transaction initiated by the calling component.
            // Here we call resume on the TM with the inbound Tranaction obj, get an XA connection, insert event into database
            // and then call suspend on the TM. If there is an error on database insertion, we setRollbackOnly() and 
            // call suspend on the TM. Based on these responses either a DONE or ERROR status will be send back(for InOnly ME)
            // to the calling component. The calling component would either commit or rollback based on the DONE or ERROR.
            Transaction tx = (Transaction) inOnly.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
            TransactionManager tm = mExtendedContext.getTransactionManager();
            try {
                tm.resume(tx);
                con = Util.getXaConnection(mExtendedContext.getConfigProperties());
                input(con, rowList);
                tm.suspend();
            } catch (Throwable t) {
                try {
                    tx.setRollbackOnly();
                    tm.suspend();
                } catch (Exception e) {}    
                mLogger.log(Level.SEVERE, mMessages.getString("InputOperation.Roll_back_event_collection", inOnly.getExchangeId()), t);
                String actor = mExtendedContext.getComponentContext().getComponentName();
                String msg = mMessages.getString("InputOperation.Fail_to_insert_data_using_XA_transaction", inOnly.getExchangeId());
                Exception e = new ServerException(msg);
                sendError(channel, inOnly, actor, FaultCode.Client, e);
            } finally {
                Util.close(con);
            }
            try {
                inOnly.setStatus(ExchangeStatus.DONE);
                mExtendedContext.getDeliveryChannel().send(inOnly);
            } catch (Exception e) {}
            return;
        } 
        addBatch(inOnly, rowList);
        return;
    }

    public void batchProcess() {
        if (mDataList.isEmpty()) {
            return;
        }
        Connection con = null;
        PreparedStatement insertStmt = null;
        String queueName = mOp.getQueueName();
        DeliveryChannel channel = mExtendedContext.getDeliveryChannel();
        try {
            con = Util.getConnection(mExtendedContext.getConfigProperties());
            insertStmt = ((Inserter) mOp).getInsertStatement(con, true);
            insertStmt.clearBatch();
            for (int i = 0, I = mDataList.size(); i < I; i++) {
                List<Object[]> rowList = mDataList.get(i);
                for (int j = 0, J = rowList.size(); j < J; j++) {
                    Object[] row = (Object[]) rowList.get(j);
                    for (int k = 0; k < row.length; k++) {
                        insertStmt.setObject(k + 1, row[k]);
                    }
                    insertStmt.addBatch();
                }
            } 
            insertStmt.executeBatch();
        } catch (Exception e) {
            String msg = mMessages.getString("InputOperation.Fail_to_insert_data_into_table", queueName);
            mLogger.log(java.util.logging.Level.SEVERE, msg, e);
            if(e instanceof SQLException){
            mLogger.log(java.util.logging.Level.SEVERE, msg, ((SQLException)e).getNextException());
            }
            String actor = mExtendedContext.getComponentContext().getComponentName();
            Exception se = new ServerException(msg);
            for (int i = 0, I = mInOnlyList.size(); i < I; i++) {
                InOnly inOnly = mInOnlyList.get(i);
                sendError(channel, inOnly, actor, FaultCode.Server, se);
            }
            clearBatch();
            return;
        } finally {
            Util.close(insertStmt);
            Util.close(con);
        }        
        for (int i = 0, I = mInOnlyList.size(); i < I; i++) {
            InOnly inOnly = mInOnlyList.get(i);
            try {
                inOnly.setStatus(ExchangeStatus.DONE);
                channel.send(inOnly);
            } catch (Exception e){}    
        }    
        clearBatch();
    }
    
    private void clearBatch() {
        mInOnlyList.clear();
        mDataList.clear();
    }

}
