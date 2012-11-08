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
 * @(#)WSDLExtensionValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validator;

import com.sun.jbi.management.descriptor.EndpointIdentifier;
import com.sun.jbi.wsdlvalidator.ValidationException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.wsdl.Binding;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.soap.SOAPBinding;

import com.sun.jbi.httpsoapbc.validator.mimevalidators.WSIAP10Validator;
import com.sun.jbi.internationalization.Messages;
import java.util.logging.Level;
import javax.wsdl.Operation;
import javax.xml.namespace.QName;
import javax.wsdl.extensions.http.HTTPBinding;

/**
 *
 * @author Administrator
 */
public class WSDLExtensionValidator {
    
    private Map mDefs;
    private EndpointIdentifier[] mSvcs;
    private WSIAP10Validator mWAIAPValidator;
    
    private static final Messages mMessages =
            Messages.getMessages(WSDLExtensionValidator.class);
    
    /** Creates a new instance of WSDLExtensionValidator 
     * @param defs 
     * @param svcs 
     */
    public WSDLExtensionValidator(Map defs, EndpointIdentifier[] svcs) {
        mDefs = defs;
        mSvcs = svcs;
        mWAIAPValidator = new WSIAP10Validator();
    }
    
    /**
     * 
     * @throws com.sun.jbi.wsdlvalidator.ValidationException 
     */
    public void validate() throws ValidationException {
        validateBindings();
    }
    
    /**
     *
     * @throws com.sun.jbi.wsdlvalidator.ValidationException
     */
    public void validateBindings() throws ValidationException {
        if ((mDefs != null) && (mSvcs != null)) {
            Iterator it = mDefs.entrySet().iterator();
            while (it.hasNext()) {
                Map.Entry entry = (Map.Entry)it.next();
                Definition def = (Definition)entry.getValue();
                for (int ii = 0; ii < mSvcs.length; ii++) {
                    EndpointIdentifier epDesc = mSvcs[ii];
                    if (epDesc == null) continue;
                    Binding binding = getBinding(def, epDesc.getServiceName().toString(), epDesc.getEndpointName());
                    if(binding != null) {
                        List <ExtensibilityElement> elems = binding.getExtensibilityElements();
                        int numSoapBindings = 0;
                        int numHttpBindings = 0;
                        for (ExtensibilityElement el : elems) {
                            if(SOAPBinding.class.isInstance(el)) {
                                numSoapBindings++;
                            } else if(HTTPBinding.class.isInstance(el)) {
                                numHttpBindings = 0;
                            }
                        }
                        if((numSoapBindings > 0) && (numSoapBindings > 1)) {
                            throw new ValidationException(mMessages.getString("HTTPBC-E00261.Element_one_per_binding", "soap:binding"));
                        }
                        if((numHttpBindings > 0) && (numHttpBindings > 1)) {
                            throw new ValidationException(mMessages.getString("HTTPBC-E00261.Element_one_per_binding", "http:binding"));
                        }
                        validateBindingOperations(binding);
                        mWAIAPValidator.validate(binding);
                    }
                    
                }
            }
        }
    }
    
    /**
     * 
     * @param binding 
     * @throws com.sun.jbi.wsdlvalidator.ValidationException 
     */
    public void validateBindingOperations(Binding binding) throws ValidationException {
        PortType pt = binding.getPortType();
        List <BindingOperation> bos = binding.getBindingOperations();
        if ((bos != null) && !(bos.isEmpty())) {
            List <Operation> ptOps = pt.getOperations();
            for(BindingOperation bo : bos) {
                boolean matchingOperationFound = false;
                for(Operation ptOp : ptOps) {
                    if ((!ptOp.isUndefined()) && (ptOp.getName().equals(bo.getName()))) {
                        matchingOperationFound = true;
                        break;
                    }
                }
                if(!matchingOperationFound) {
                    throw new ValidationException(mMessages.getString("HTTPBC-E00262.Binding_op_must_match_porttype_op",
                            new Object[]{ bo.getName(),
                            binding.getQName().getLocalPart(),
                            pt.getQName().getLocalPart()}
                    ));
                }
            }
        }
    }
    /**
     * 
     * @param def 
     * @param serviceName 
     * @param endpointName 
     * @return 
     */
    public Binding getBinding(Definition def, String serviceName, String endpointName) {
        String location = null;
        Service svc = def.getService(QName.valueOf(serviceName));
        if (svc == null) {
            return null;
        }
        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port == null) {
            return null;
        } else {
            return port.getBinding();
        }
    }
}
