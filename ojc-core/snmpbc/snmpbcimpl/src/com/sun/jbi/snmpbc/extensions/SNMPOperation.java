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
 * @(#)SNMPOperation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc.extensions;

import java.io.Serializable;
import javax.wsdl.BindingOperation;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;


public class SNMPOperation implements ExtensibilityElement, Serializable {

    public static final String IN_ONLY = "http://www.w3.org/2004/08/wsdl/in-only";
    public static final String IN_OUT = "http://www.w3.org/2004/08/wsdl/in-out";
    public static final String IN_OPTIONAL_OUT = "http://www.w3.org/2004/08/wsdl/in-opt-out";
    public static final String ROBUST_IN_ONLY = "http://www.w3.org/2004/08/wsdl/robust-in-only";
    public static final String OUT_ONLY = "http://www.w3.org/2004/08/wsdl/out-only";
    public static final String OUT_IN = "http://www.w3.org/2004/08/wsdl/out-in";
    public static final String OUT_OPTIONAL_IN = "http://www.w3.org/2004/08/wsdl/out-opt-in";
    public static final String ROBUST_OUT_ONLY = "http://www.w3.org/2004/08/wsdl/robust-out-only";    

    // snmp
    public static enum SnmpOpType { MOF, ADAPTATION, PM;
        
        public static SnmpOpType parse(String s) throws IllegalArgumentException {
            if ("mof".equals(s)) {
                return MOF;
            } else if ("adaptation".equals(s)) {
                return ADAPTATION;
            } else if ("pm".equals(s)) {
                return PM;
            } else {
                throw new IllegalArgumentException("Parsing error encounter for SnmpOpType: " +
                        s);
            }
        }
    }
    public static final String ATTR_TYPE = "type";
    public static final String ATTR_MOF_ID = "mofId";
    public static final String ATTR_ADAPTATION_ID = "adaptationId";
    public static final String ATTR_MOF_ID_REF = "mofIdRef";
    
    
    // nested elements    
    private SnmpOpType type;
    private String mofId;
    private String adaptationId;
    private String mofIdRef;
    
    private QName fieldElementType = SNMPConstants.QNAME_OPERATION;

    private Boolean fieldRequired = null;
    
    private BindingOperation mBindingOp;
    private String mep;
    
    public SNMPOperation() {
    }

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
     * @return 
     */
    public Boolean getRequired() {
        return fieldRequired;
    }

    /**
     * Set whether required (for wsdl:required)
     * @param required 
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }
    
    public void setBindingOperation (BindingOperation bindingOp) {
        this.mBindingOp = bindingOp;
    }
    
    public BindingOperation getBindingOperation() {
        return mBindingOp;
    }
    
    public void setMEP (String mep) {
        this.mep = mep;
    }
    
    public String getMEP () {
        return mep;
    }
    
    public SnmpOpType getType() {
        return type;
    }
    
    public void setType(SnmpOpType val) {
        type = val;
    }
    
    public String getMofId() {
        return mofId;
    }
    
    public void setMofId(String val) {
        mofId = val;
    }
    
    public String getAdaptationId() {
        return adaptationId;
    }
    
    public void setAdaptationId(String val) {
        adaptationId = val;
    }
    
    public String getMofIdRef() {
        return mofIdRef;
    }
    
    public void setMofIdRef(String val) {
        mofIdRef = val;
    }
    
    
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nSNMP Operation (" + fieldElementType + "):");
        strBuf.append("\nBindingOperation=" + (mBindingOp==null?"not known yet" : mBindingOp.toString()));
        strBuf.append("\nMEP="+mep);
        return strBuf.toString();
    }
}
