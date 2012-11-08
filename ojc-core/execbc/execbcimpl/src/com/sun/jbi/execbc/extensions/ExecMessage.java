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
 * @(#)ExecMessage.java 
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
public class ExecMessage implements ExtensibilityElement, Serializable {
    private static final long serialVersionUID = 1L;
    public static final String EXEC_USE_TYPE_LITERAL = "literal";
    public static final String EXEC_USE_TYPE_ENCODED = "encoded";
    private QName fieldElementType = ExecConstants.QNAME_MESSAGE;
    private Boolean fieldRequired;
    private String execEncodingStyle;
    private String execUseType = EXEC_USE_TYPE_LITERAL; // default
    private int recordsToBeSkipped;
    private Delimiters delimiters;
    private boolean injectContextInfo = true;
    
    public ExecMessage() {}
    
    /**
     * Set the extensibility element type
     * @param elementType the type
     */
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
    }
    
    /**
     * Get the extensibility element type
     * @return the extensibility element's type
     */
    public QName getElementType() {
        return fieldElementType;
    }
    
    /**
     * Set whether required (for wsdl:required)
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }
    
    /**
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return fieldRequired;
    }
    
    public void setExecUseType(String val) {
        execUseType = val;
    }
    
    public String getExecUseType() {
        return execUseType;
    }
    
    public void setExecEncodingStyle(String val) {
        execEncodingStyle = val;
    }
    
    public String getExecEncodingStyle() {
        return execEncodingStyle;
    }
    
    public int getRecordsToBeSkipped() {
        return recordsToBeSkipped;
    }
    
    public void setRecordsToBeSkipped(int val) {
        recordsToBeSkipped = val;
    }
    
    public Delimiters getDelimitersOfRecord() {
        return delimiters;
    }
    
    public void setDelimitersOfRecord(Delimiters val) {
        delimiters = val;
    }
    
    public void setDelimitersOfRecord(String desc) {
        delimiters = new Delimiters(desc);
    }
    
    public boolean getInjectContextInfo() {
        return injectContextInfo;
    }
    
    public void setInjectContextInfo(boolean val) {
        injectContextInfo = val;
    }
    
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nExec Message (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        strBuf.append("\nUse=" + execUseType);
        strBuf.append("\nEncoding Style=" + execEncodingStyle);
        
        return strBuf.toString();
    }
}
