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
 * @(#)JDBCSql.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.extensions;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;


/**
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class JDBCSql implements ExtensibilityElement, Serializable {
    private static final long serialVersionUID = 1L;
    public static final String ATTR_PARAM_ORDER = "paramOrder";
    public static final String ATTR_SQL = "sql";
    public static final String PK_NAME = "PKName";
    public static final String MARK_COLUMN_NAME = "MarkColumnName";

    //	public static final String STAG_SELECT = "CreateStageTable";
    public static final String TABLE_NAME = "TableName";
    public static final String MOVE_TABLE_NAME = "MoveRowToTableName";
    public static final String POLLING_POST_PROCESS = "PollingPostProcessing";
    public static final String MARK_COLUMN_VALUE = "MarkColumnValue";
    public static final String TRANSACTION = "Transaction";
    private QName mFieldElementType = JDBCConstants.QNAME_SQL;
    private Boolean mFieldRequired = null;
    private String mParamOrder = null;
    private String mSql = null;
    private String mMarkColumnName = null;
    private String mMarkColumnValue = null;
    private String mpkName = null;
    private String mTableName = null;
    private String mMoveRowToTableName = null;
    private String mPollingPostProcessing = null;
    private String mTransaction = "NOTransaction";

    public JDBCSql() {
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

    protected void setParamOrder(final String paramOrder) {
        mParamOrder = paramOrder;
    }

    /**
     *
     * @return mParamOrder
     */
    protected String getParamOrder() {
        return mParamOrder;
    }

    protected void setSql(final String sql) {
        mSql = sql;
    }

    /**
     *
     * @return mSql
     */
    protected String getSql() {
        return mSql;
    }

    /**
     *
     * @return mMarkColumnName
     */
    protected String getMarkColumnName() {
        return mMarkColumnName;
    }

    /**
     *
     * 
     * @param markcolumnName 
     */
    protected void setMarkColumnName(final String markcolumnName) {
        mMarkColumnName = markcolumnName;
    }

    /**
     *
     * @return mMarkColumnName
     */
    protected String getMarkColumnValue() {
        return mMarkColumnValue;
    }

    /**
     *
     * @param MarkColumnValue
     */
    protected void setMarkColumnValue(final String MarkColumnValue) {
        mMarkColumnValue = MarkColumnValue;
    }

    /**
     *
     * @return mpkName
     */
    protected String getPKName() {
        return mpkName;
    }

    protected void setPKName(final String pkName) {
        mpkName = pkName;
    }

    /**
     *
     * @return mTableName
     */
    protected String getTableName() {
        return mTableName;
    }

    protected void setTableName(final String tableName) {
        mTableName = tableName;
    }

    /**
     *
     * @return mMoveRowToTableName
     */
    protected String getMoveRowToTableName() {
        return mMoveRowToTableName;
    }

    protected void setMoveRowToTableName(final String MoveRowToTableName) {
        mMoveRowToTableName = MoveRowToTableName;
    }

    /**
     *
     * @return mPollingPostProcessing
     */
    protected String getPollingPostProcessing() {
        return mPollingPostProcessing;
    }

    private void setPollingPostProcessing(final String PollingPostProcessing) {
        mPollingPostProcessing = PollingPostProcessing;
    }

    /**
     *
     * @return mTransaction
     */
    protected String getTransaction() {
        return this.mTransaction;
    }

    protected void setTransaction(final String Transaction) {
        this.mTransaction = Transaction;
    }

    /**
     *
     * @return mStagName
     */

    //	public String getCreateStageTable(){
    //		return this.mStagName;

    //	}

    //	public void setCreateStageTable(String stagName){
    //		this.mStagName = stagName;
    //	}
}
