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
 * @(#)JDBCSPOperationInput.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.extensions;

import java.io.Serializable;

import javax.wsdl.Message;
import javax.wsdl.extensions.ExtensibilityElement;

import javax.xml.namespace.QName;


/**
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class SPOperationInput implements ExtensibilityElement, Serializable {
    public static final String ATTR_OPERATION_TYPE = "operationType";
    private static final long serialVersionUID = 11L;

    /**Changes**/
    public static final String ATTR_SPNAME = "ProcedureName";
    public static final String ATTR_SP_EXEC_STRING = "ExecutionString";
    public static final String TRANSACTION = "Transaction";

    private QName mFieldElementType = JDBCConstants.QNAME_SP_OPERATION_INPUT;
    private Boolean mFieldRequired = null;
    private String mOperationType = null;
    public SPOperationInput mJdbcInput = null;
    private Message mMessage = null;
    private String mInputName = null;

    private String mExecutionString = null;
    private String mProcedureName = null;
    private String mTransaction = "NOTransaction";

    /***Changes**/
    public SPOperationInput() {
    }

    public void setProcedureName(final String procName) {
        mProcedureName = procName;
    }

    /**
     *
     * @return mParamOrder
     */
    public String getProcedureName() {
        return mProcedureName;
    }

    public void setExecutionString(final String spExecutionString) {
        mExecutionString = spExecutionString;
    }

    /**
     *
     * @return mSql
     */
    public String getExecutionString() {
        return mExecutionString;
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

    public void setSPInput(final SPOperationInput jdbcInput) {
        mJdbcInput = jdbcInput;
    }

    public SPOperationInput getSPInput() {
        return mJdbcInput;
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

}
