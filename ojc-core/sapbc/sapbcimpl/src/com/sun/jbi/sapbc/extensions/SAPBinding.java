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
 * @(#)SAPBinding.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.extensions;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.sapbc.util.SAPWSDLUtilities;
import com.sun.wsdl.model.WSDLDefinitions;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.extensions.impl.ExtensibilityElementImpl;
import java.io.PrintWriter;
import java.io.Serializable;
import java.util.Map;
import java.util.logging.Logger;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;

public class SAPBinding extends ExtensibilityElementImpl implements Serializable {
    
    public SAPBinding(final ExtensibilityElement el, final SAPEnvironmentalVars sapEnvVar) throws WSDLException {
        fromXML(el, sapEnvVar);
    }
    
    public QName getElementType() {
        return fieldElementType;
    }
    
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
    }
    
    public Boolean getRequired() {
        return fieldRequired;
    }
    
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }
    
    public TransactionalMode transactionalMode() {
        return transactionalMode;
    }
    
    public String transactionIdDatabase() {
        return transactionIdDb;
    }
    
    public Long maxTransactionIdRows() {
        return maxTransactionIdRows;
    }
    
    public String toString() {
        StringBuffer strBuf = new StringBuffer();
        strBuf.append("SAPBinding " + fieldElementType.toString() + ":");
        strBuf.append("\nRequired: " + fieldRequired);
        strBuf.append("\n" + SAPBinding.ATTR_TRANSACTIONAL_MODE + ": " + transactionalMode);
        strBuf.append("\n" + SAPBinding.ATTR_TRANSACTION_ID_DB + ": " + transactionIdDb);
        strBuf.append("\n" + SAPBinding.ATTR_MAX_TID_ROWS + ": " + maxTransactionIdRows);
        return strBuf.toString();
    }
    
    public void toXML(final WSDLDefinitions def, final PrintWriter pw)
    throws WSDLException {
        
        pw.print("    <sap:binding");
        
        if (fieldRequired != null) {
            SAPWSDLUtilities.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, fieldRequired.toString(), def, pw);
        }
        DOMUtils.printAttribute(
                SAPBinding.ATTR_TRANSACTIONAL_MODE,
                transactionalMode().toString(),
                pw);
        
        DOMUtils.printAttribute(
                SAPBinding.ATTR_TRANSACTION_ID_DB,
                transactionIdDatabase().toString(),
                pw);
        
        DOMUtils.printAttribute(
                SAPBinding.ATTR_MAX_TID_ROWS,
                maxTransactionIdRows().toString(),
                pw);
        
        pw.print("/>\n");
    }
    
    public void fromXML(final ExtensibilityElement el, final SAPEnvironmentalVars sapEnvVar) throws WSDLException {
        String val;
        Map attrMap = el.getOtherAttributes();
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPBinding.ATTR_TRANSACTIONAL_MODE);
        if (val != null) {
            TransactionalMode t = TransactionalMode.forDisplayName(val);
            if (t != null) {
                transactionalMode = t;
            } else {
                String msg = mMessages.getString(
                        "SAPBinding.Unknown_value_for_attr",
                        new Object[] { SAPBinding.ATTR_TRANSACTIONAL_MODE, val });
                throw new WSDLException("", msg);
            }
        } else {
            String msg = mMessages.getString(
                    "SAPBinding.Missing_required_attr",
                    SAPBinding.ATTR_TRANSACTIONAL_MODE);
            throw new WSDLException("", msg);
        }
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPBinding.ATTR_TRANSACTION_ID_DB);
        if (val != null) {
            transactionIdDb = val;
        }
        
        val = (String) sapEnvVar.getAttrAndResolveEnvVar(attrMap, SAPBinding.ATTR_MAX_TID_ROWS);
        if (val != null) {
            maxTransactionIdRows = Long.valueOf(val);
        }
    }
    
    private static final long serialVersionUID = 1L;
    private static final Messages mMessages = Messages.getMessages(SAPBinding.class);
    private static final Logger mLogger = Messages.getLogger(SAPBinding.class);
    protected static final String ATTR_TRANSACTIONAL_MODE = "transactionalMode"; // NO I8N
    protected static final String ATTR_TRANSACTION_ID_DB = "transactionIDVerificationDatabase"; // NO I8N
    protected static final String ATTR_MAX_TID_ROWS = "maxTIDDatabaseRows"; // NO I8N
    
    private QName fieldElementType = TYPE;
    private Boolean fieldRequired = null;
    private TransactionalMode transactionalMode = TransactionalMode.NONTRANSACTIONAL;
    private String transactionIdDb = "";
    private Long maxTransactionIdRows = 0L;
    
    public static final QName TYPE =
            new QName(SAPWsdlConstants.EXT_NAMESPACE_URI, Constants.ELEM_BINDING, "sap");
    
    public enum TransactionalMode {
        TRANSACTIONAL    ("Transactional"),    // NO I8N
        NONTRANSACTIONAL("Non-Transactional"); // NO I8N
        
        public static TransactionalMode forDisplayName(String val) {
            for (TransactionalMode t : values()) {
                if (t.toString().equals(val)) {
                    return t;
                }
            }
            return null;
        }
        
        TransactionalMode(String displayName) {
            this.displayName = displayName;
        }
        public String toString() {
            return displayName;
        }
        private final String displayName;
    }
    
}
