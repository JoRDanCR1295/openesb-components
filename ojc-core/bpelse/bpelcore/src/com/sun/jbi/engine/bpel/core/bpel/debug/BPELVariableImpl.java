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
 * @(#)BPELVariableImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.debug;

import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELVariable;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.WSDLMessage;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;

/**
 * Implementatin of BPELVariable for debugger to query variables in target
 * BPEL engine.
 * 
 * @author Sun Microsystems
 * @version 
 */
public class BPELVariableImpl implements BPELVariable {   
    private boolean isWSDLMessage;
    private RVariable mModel;
    private RuntimeVariable mData;   
    private String mXSDData;
    private WSDLMessage mWSDLMsg;
    
     /** 
      * Creates a new instance of BPELVariableImpl 
      * @param model    model of the runtime variable
      * @param data     data of the runtime variable
      **/
    public BPELVariableImpl(RVariable model, RuntimeVariable data) {
    	if (model == null) {
    		throw new NullPointerException("runtime variable data is null");
    	}
        isWSDLMessage = (model.getWSDLMessageType() != null);
        mModel = model;
        mData = data;        
    }
    
    /**
     * Creates a new instance of BPELVariableImpl for given non-null xsd data
     * @param  xsdData   xsd string data of the runtime variable
     * 
     **/
    public BPELVariableImpl(String xsdData) {
        if (xsdData == null) {
            throw new NullPointerException("xsd data is null");
        }
        mXSDData = xsdData;
        isWSDLMessage = false;
    }
    /**
     * Creates a new instance of BPELVariableImpl for given non-null WSDL message
     * @param  msg     value of WSDL message 
     **/      
    public BPELVariableImpl(WSDLMessage msg) {
        if (msg == null) {
            throw new NullPointerException("wsdlmessage is null");
        }
        mWSDLMsg = msg;
        isWSDLMessage = true;
    }
    
    /**
     * Checks if this BPEL variable is WSDL message.
     */
    public boolean isWSDLMessage() {        
        return isWSDLMessage;
    }
    /**
     * Returns the WSDL message held in this variable. 
     * @return  the WSDL message content.
     */
    public WSDLMessage getWSDLMessage() {
    	if (mXSDData != null) {
            return null;
    	}
        
        if (mWSDLMsg != null) {
            return mWSDLMsg;
        } else if (isInitialized() && (mData.getWSMessage() != null)) {
            return new WSDLMessageImpl(
                    mModel.getWSDLMessageType(), mData.getWSMessage());   
        }
        
        return null;
    }
    
    /**
     * Returns the XSD type string serialization. For XSD simple type variable, it returns
     * the CII which represents the simple type value. For complex type and element, it
     * returns the string value of the EII.
     * @return xsd data
     */
    public String getXSDData() {
        String result = null;
        
    	if (mWSDLMsg != null) {
            return null;
    	}
        
        if (mXSDData != null) {
            return mXSDData;
        } else if (isInitialized ()) {    
            final Object variableData = mData.getXSDVariableData();
            
            if (variableData != null) {
                if (variableData instanceof Element) {
                    result = DOMHelper.createXmlString((Element) variableData);
                } else if (variableData instanceof Document) {
                    result = DOMHelper.createXmlString(((Document) variableData)
                            .getDocumentElement());
                } else {
                    result = variableData.toString();
                }
            }
        }
        
        return result; 
    }

    public boolean isBoolean() {
        // TODO Auto-generated method stub
        return mModel.isBoolean();
    }

    public boolean isNumber() {
        // TODO Auto-generated method stub
        return mModel.isNumber();
    }

    public boolean isSimpleType() {
        // TODO Auto-generated method stub
        return isBoolean () || isNumber () || isString ();
    }

    public boolean isString() {
        // TODO Auto-generated method stub
        return mModel.isString();
    }

    public boolean isInitialized() {
        // TODO Auto-generated method stub
        return mData != null || mXSDData != null || mWSDLMsg != null;
    }
       
}
