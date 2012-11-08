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
 * @(#)ExecOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 * @author Sherry Weng
 * @author Jun Xu
 */
public class ExecOperation implements ExtensibilityElement, Serializable {
    private static final long serialVersionUID = 1L;
    
    QName fieldElementType = ExecConstants.QNAME_OPERATION;
    ExecInput execInput = null;
    ExecOutput execOutput = null;
    
    Boolean fieldRequired = Boolean.FALSE;
    String command;
    int pollingInterval;
    PollingPattern pollingPattern =
        PollingPattern.REPETITIVE_INVOKE_AND_RECEIVE;
    
    public ExecOperation() {}
    
    /**
     * Get the extensibility element type
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return fieldElementType;
    }
    
    /**
     * Set the extensibility element type
     * @param elementType the type
     */
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
    }
    
    /**
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return fieldRequired;
    }
    
    /**
     * Set whether required (for wsdl:required)
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }
    
    /**
     * Set the Input element properties for current Exec BC Operation
     */
    public void setExecOperationInput(ExecInput input) {
        execInput = input;
    }
    
    /**
     * Get the Input element properties for current Exec BC Operation
     */
    public ExecInput getExecOperationInput() {
        return execInput;
    }
    
    /**
     * Set the Input element properties for current Exec BC Operation
     */
    public void setExecOperationOutput(ExecOutput output) {
        execOutput = output;
    }
    
    /**
     * Get the Input element properties for current Exec BC Operation
     */
    public ExecOutput getExecOperationOutput() {
        return execOutput;
    }
    
    /**
     * Gets the command used in exec.
     * 
     * @return The command used in exec.
     */
    public String getCommand() {
        return command;
    }
    
    /**
     * Sets the command used in exec.
     * 
     * @param val The command used in exec
     */
    public void setCommand(String val) {
        command = val;
    }
    
    /**
     * Gets the interval in seconds to execute the command.
     * 
     * @return The interval in seconds to execute the command.
     */
    public int getPollingInterval() {
        return pollingInterval;
    }
    
    /**
     * Sets the interval in seconds to execute the command.
     * 
     * @param val The interval in seconds to execute the command.
     */
    public void setPollingInterval(int val) {
        pollingInterval = val;
    }
    
    /**
     * Gets the polling pattern.
     * 
     * @return the polling pattern.
     */
    public PollingPattern getPollingPattern() {
        return pollingPattern;
    }
    
    /**
     * Sets the polling pattern.
     * 
     * @param val the polling pattern
     */
    public void setPollingPattern(PollingPattern val) {
        pollingPattern = val;
    }
    
    /**
     * Sets the polling pattern via string
     * 
     * @param val the polling pattern in string
     */
    public void setPollingPattern(String val) {
        if ("RepetitiveInvokeAndReceive".equals(val)) {
            pollingPattern = PollingPattern.REPETITIVE_INVOKE_AND_RECEIVE;
        } else if ("InvokeOnceAndKeepReceiving".equals(val)) {
            pollingPattern = PollingPattern.INVOKE_ONCE_AND_KEEP_RECEIVING;
        } else {
            throw new IllegalArgumentException("Illegal enum value: '"
                    + val + "'");
        }
    }
    
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nExec operation (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        
        return strBuf.toString();
    }
}
