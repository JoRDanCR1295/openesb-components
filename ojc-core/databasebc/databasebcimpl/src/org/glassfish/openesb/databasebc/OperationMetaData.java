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
 * @(#)OperationMetaData.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc;

import org.glassfish.openesb.databasebc.extensions.JDBCOperation;
import org.glassfish.openesb.databasebc.extensions.JDBCOperationInput;
import org.glassfish.openesb.databasebc.extensions.JDBCOperationOutput;
import org.glassfish.openesb.databasebc.extensions.SPOperationInput;
import org.glassfish.openesb.databasebc.extensions.SPOperationOutput;

import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.Operation;


/**
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class OperationMetaData {
    private JDBCOperation mJDBCOperation;
    private JDBCOperationInput mJDBCOperationInput;
    private JDBCOperationOutput mJDBCOperationOutput;
    private JDBCOperationInput mJdbcSql;
    private SPOperationInput mJDBCSPOperationInput;
    private SPOperationOutput mJDBCSPOperationOutput;
    private SPOperationInput mSPInput;
    
    private Operation mOperation;
    private BindingOperation mBindingOperation;
    private Definition mDefinition;

    public OperationMetaData() {
    }

    /**
     *
     * @param jdbcOperation
     */
    public void setJDBCOperation(final JDBCOperation jdbcOperation) {
        mJDBCOperation = jdbcOperation;
    }

    /**
     *
     * @return
     */
    public JDBCOperation getJDBCOperation() {
        return mJDBCOperation;
    }

    /**
     *
     * @param operationInput
     */
    public void setJDBCOperationInput(final JDBCOperationInput operationInput) {
        mJDBCOperationInput = operationInput;
    }

    /**
     *
     * @return
     */
    public JDBCOperationInput getJDBCOperationInput() {
        return mJDBCOperationInput;
    }

    /**
     *
     * @param operationInput
     */
    public void setJDBCSPOperationInput(final SPOperationInput operationInput) {
        mJDBCSPOperationInput = operationInput;
    }

    /**
     *
     * @return
     */
    public SPOperationInput getJDBCSPOperationInput() {
        return mJDBCSPOperationInput;
    }    
    /**
     *
     * @param jdbcSql
     */
    public void setJDBCSql(final JDBCOperationInput jdbcSql) {
        mJdbcSql = jdbcSql;
    }

    /**
     *
     * @return
     */
    public JDBCOperationInput getJDBCSql() {
        return mJdbcSql;
    }

    /**
     *
     * @param operationOutput
     */
    public void setJDBCOperationOutput(final JDBCOperationOutput operationOutput) {
        mJDBCOperationOutput = operationOutput;
    }

    /**
     *
     * @return
     */
    public JDBCOperationOutput getJDBCOperationOutput() {
        return mJDBCOperationOutput;
    }

    /**
     *
     * @param operationOutput
     */
    public void setJDBCSPOperationOutput(final SPOperationOutput operationOutput) {
        mJDBCSPOperationOutput = operationOutput;
    }

    /**
     *
     * @return
     */
    public SPOperationOutput getJDBCSPOperationOutput() {
        return mJDBCSPOperationOutput;
    }
    
    /**
     *
     * @param bindingOperation
     */
    protected void setBindingOperation(final BindingOperation bindingOperation) {
        mBindingOperation = bindingOperation;
        mOperation = bindingOperation.getOperation();
    }

    /**
     *
     * @return
     */
    public BindingOperation getBindingOperation() {
        return mBindingOperation;
    }

    /**
     *
     * @param operation
     */
    public void setOperation(final Operation operation) {
        mOperation = operation;
    }

    /**
     *
     * @return
     */
    Operation getOperation() {
        return mOperation;
    }

    /**
     *
     * @param definition
     */
    protected void setDefinition(final Definition definition) {
        mDefinition = definition;
    }

    void setSPInput(SPOperationInput spInput) {
        this.mSPInput = spInput;
    }

    /**
     *
     * @return
     */
    private Definition getDefinition() {
        return mDefinition;
    }
}
