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
 * @(#)JDBCOperationInput.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc.extensions;

import java.io.Serializable;

import javax.wsdl.Message;
import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;


/**
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class JDBCOperationInput implements ExtensibilityElement, Serializable {
    public static final String ATTR_OPERATION_TYPE = "operationType";
    public static final String ATTR_NUMBER_OF_RECORDS = "numberOfRecords";
    private static final long serialVersionUID = 1L;

    /**Changes**/
    public static final String ATTR_PARAM_ORDER = "paramOrder";
    public static final String ATTR_SQL = "sql";
    public static final String PK_NAME = "PKName";
    public static final String MARK_COLUMN_NAME = "MarkColumnName";
    public static final String TABLE_NAME = "TableName";
    public static final String MOVE_TABLE_NAME = "MoveRowToTableName";
    public static final String POLLING_POST_PROCESS = "PollingPostProcessing";
    public static final String MARK_COLUMN_VALUE = "MarkColumnValue";
    public static final String TRANSACTION = "Transaction";
    public static final String POLLMILLISECONDS = "PollMilliSeconds";

    private QName mFieldElementType = JDBCConstants.QNAME_OPERATION;
    private Boolean mFieldRequired = null;
    private String mOperationType = null;
    public JDBCOperationInput mJdbcSql = null;
    private Message mMessage = null;
    private int numberOfRecords = -1;
	private int pollmilliseconds = 10000;
    private String mInputName = null;

    //private QName mFieldElementType = JDBCConstants.QNAME_SQL;
    private String mParamOrder = null;
    private String mSql = null;
    private String mMarkColumnName = null;
    private String mMarkColumnValue = null;
    private String mpkName = null;
    private String mTableName = null;
    private String mMoveRowToTableName = null;
    private String mPollingPostProcessing = null;
    private String mTransaction = "NOTransaction";

    /***Changes**/
    public JDBCOperationInput() {
    }

    public void setParamOrder(final String paramOrder) {
        mParamOrder = paramOrder;
    }

    /**
     *
     * @return mParamOrder
     */
    public String getParamOrder() {
        return mParamOrder;
    }

    public void setSql(final String sql) {
        mSql = sql;
    }

    /**
     *
     * @return mSql
     */
    public String getSql() {
        return mSql;
    }

    /**
     *
     * @return mMarkColumnName
     */
    public String getMarkColumnName() {
        return mMarkColumnName;
    }

    /**
     *
     * 
     * @param markcolumnName 
     */
    public void setMarkColumnName(final String markcolumnName) {
        mMarkColumnName = markcolumnName;
    }

    /**
     *
     * @return mMarkColumnName
     */
    public String getMarkColumnValue() {
        return mMarkColumnValue;
    }

    /**
     *
     * @param MarkColumnValue
     */
    public void setMarkColumnValue(final String markColumnValue) {
        mMarkColumnValue = markColumnValue;
    }

    /**
     *
     * @return mpkName
     */
    public String getPKName() {
        return mpkName;
    }

    public void setPKName(final String pkName) {
        mpkName = pkName;
    }

    /**
     *
     * @return mTableName
     */
    public String getTableName() {
        return mTableName;
    }

    public void setTableName(final String tableName) {
        mTableName = tableName;
    }

    /**
     *
     * @return mMoveRowToTableName
     */
    public String getMoveRowToTableName() {
        return mMoveRowToTableName;
    }

    protected void setMoveRowToTableName(final String moveRowToTableName) {
        mMoveRowToTableName = moveRowToTableName;
    }

    /**
     *
     * @return mPollingPostProcessing
     */
    public String getPollingPostProcessing() {
        return mPollingPostProcessing;
    }

    public void setPollingPostProcessing(final String pollingPostProcessing) {
        mPollingPostProcessing = pollingPostProcessing;
    }

    /**
     *
     * @return mTransaction
     */
    public String getTransaction() {
        return this.mTransaction;
    }

    public void setTransaction(final String transaction) {
        this.mTransaction = transaction;
    }

    /**
     * @return mFieldElementType
     */
    //@Override
    public QName getElementType() {
        return mFieldElementType;
    }

    /**
    * @return mFieldRequired
    */
    //@Override
    public Boolean getRequired() {
        return mFieldRequired;
    }

    //@Override
    public void setElementType(final QName elementType) {
        mFieldElementType = elementType;
    }

    //@Override
    public void setRequired(final Boolean required) {
        mFieldRequired = required;
    }

    public void setOperationType(final String operationType) {
        mOperationType = operationType;
    }

    /**
     *
     * @return mOperationType
     */
    public String getOperationType() {
        return mOperationType;
    }

    public void setJDBCSql(final JDBCOperationInput jdbcSql) {
        mJdbcSql = jdbcSql;
    }

    public JDBCOperationInput getJDBCSql() {
        return mJdbcSql;
    }

    public void setNumberOfRecords(final int numberOfRecords) {
        this.numberOfRecords = numberOfRecords;
    }

    /**
     *
     * @return numberOfRecords
     */
    public int getNumberOfRecords() {
        return numberOfRecords;
    }

    protected Message getMessage() {
        return mMessage;
    }

    public void setMessage(final Message mMessage) {
        this.mMessage = mMessage;
    }

    protected String getOperationName() {
        return mInputName;
    }

    public void setInputName(final String mInputName) {
        this.mInputName = mInputName;
    }

    public int getPollMilliSeconds() {
        return pollmilliseconds;
    }

    protected void setPollMilliSeconds(final int mPollSec) {
        this.pollmilliseconds = mPollSec;
    }
}
