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
 * @(#)HttpSoapValidatorRegistry.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validator;

import java.util.HashMap;
import java.util.Map;

import javax.wsdl.Binding;
import javax.wsdl.BindingFault;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Port;
import javax.wsdl.extensions.mime.MIMEPart;
import javax.xml.namespace.QName;

import com.ibm.wsdl.extensions.http.HTTPConstants;
import com.ibm.wsdl.extensions.soap.SOAPConstants;
import com.ibm.wsdl.extensions.soap12.SOAP12Constants;
import com.sun.jbi.httpsoapbc.configuration.RuntimeConfigurationMBean;
import com.sun.jbi.httpsoapbc.validator.httpvalidators.HttpAddressValidator;
import com.sun.jbi.httpsoapbc.validator.httpvalidators.HttpBindingValidator;
import com.sun.jbi.httpsoapbc.validator.httpvalidators.HttpOperationValidator;
import com.sun.jbi.httpsoapbc.validator.soap12validators.HttpSoap12AddressValidator;
import com.sun.jbi.httpsoapbc.validator.soap12validators.HttpSoap12BindingValidator;
import com.sun.jbi.httpsoapbc.validator.soap12validators.HttpSoap12BodyValidator;
import com.sun.jbi.httpsoapbc.validator.soap12validators.HttpSoap12FaultValidator;
import com.sun.jbi.httpsoapbc.validator.soap12validators.HttpSoap12HeaderValidator;
import com.sun.jbi.httpsoapbc.validator.soap12validators.HttpSoap12OperationValidator;
import com.sun.jbi.httpsoapbc.validator.soapvalidators.HttpSoapAddressValidator;
import com.sun.jbi.httpsoapbc.validator.soapvalidators.HttpSoapBindingValidator;
import com.sun.jbi.httpsoapbc.validator.soapvalidators.HttpSoapBodyValidator;
import com.sun.jbi.httpsoapbc.validator.soapvalidators.HttpSoapFaultValidator;
import com.sun.jbi.httpsoapbc.validator.soapvalidators.HttpSoapHeaderValidator;
import com.sun.jbi.httpsoapbc.validator.soapvalidators.HttpSoapOperationValidator;
import com.sun.jbi.wsdlvalidator.Validator;
import com.sun.jbi.wsdlvalidator.ValidatorRegistry;

public class HttpSoapValidatorRegistry implements ValidatorRegistry {

    protected Map mValidatorReg;
    protected RuntimeConfigurationMBean mRuntimeConfig;
    protected boolean mResolveTokens = false;
    
    public HttpSoapValidatorRegistry(RuntimeConfigurationMBean runtimeConfig, boolean resolveTokens) {
    	this.mRuntimeConfig = runtimeConfig;
    	this.mResolveTokens = resolveTokens;
    	
        mValidatorReg = new HashMap();
        registerHttpSoapValidators();
        registerHttpSoap12Validators();
        registerHttpValidators();
    }

    protected void registerHttpSoap12Validators() {
	HttpSoap12AddressValidator soap12AddressValidator =
            new HttpSoap12AddressValidator(mRuntimeConfig, mResolveTokens);
        registerValidator(Port.class,
                          SOAP12Constants.Q_ELEM_SOAP_ADDRESS,
                          soap12AddressValidator);
        
        HttpSoap12BindingValidator soap12BindingValidator =
            new HttpSoap12BindingValidator();
        registerValidator(Binding.class,
                          SOAP12Constants.Q_ELEM_SOAP_BINDING,
                          soap12BindingValidator);
        
        HttpSoap12HeaderValidator soap12HeaderValidator =
            new HttpSoap12HeaderValidator();
        registerValidator(BindingInput.class,
                          SOAP12Constants.Q_ELEM_SOAP_HEADER,
                          soap12HeaderValidator);
        registerValidator(BindingOutput.class,
                          SOAP12Constants.Q_ELEM_SOAP_HEADER,
                          soap12HeaderValidator);

        HttpSoap12BodyValidator soap12BodyValidator =
            new HttpSoap12BodyValidator();
        registerValidator(BindingInput.class,
                          SOAP12Constants.Q_ELEM_SOAP_BODY,
                          soap12BodyValidator);
        registerValidator(BindingOutput.class,
                          SOAP12Constants.Q_ELEM_SOAP_BODY,
                          soap12BodyValidator);
        registerValidator(MIMEPart.class,
                          SOAP12Constants.Q_ELEM_SOAP_BODY,
                          soap12BodyValidator);

        HttpSoap12FaultValidator soap12FaultValidator =
            new HttpSoap12FaultValidator();
        registerValidator(BindingFault.class,
                          SOAP12Constants.Q_ELEM_SOAP_FAULT,
                          soap12FaultValidator);

        HttpSoap12OperationValidator soap12OperationValidator =
            new HttpSoap12OperationValidator();
        registerValidator(BindingOperation.class,
                          SOAP12Constants.Q_ELEM_SOAP_OPERATION,
                          soap12OperationValidator);
        
        // Attach interested validators as observers to other validators
        soap12HeaderValidator.attachObserver(soap12BodyValidator);
        soap12BodyValidator.attachObserver(soap12HeaderValidator);
        soap12HeaderValidator.getHeaderFaultValidator().attachObserver(soap12BodyValidator);
        soap12BodyValidator.attachObserver(soap12HeaderValidator.getHeaderFaultValidator());
	
    }

    protected void registerHttpSoapValidators() {

        HttpSoapAddressValidator soapAddressValidator =
            new HttpSoapAddressValidator(mRuntimeConfig, mResolveTokens);
        registerValidator(Port.class,
                          SOAPConstants.Q_ELEM_SOAP_ADDRESS,
                          soapAddressValidator);

        HttpSoapBindingValidator soapBindingValidator =
            new HttpSoapBindingValidator();
        registerValidator(Binding.class,
                          SOAPConstants.Q_ELEM_SOAP_BINDING,
                          soapBindingValidator);

        HttpSoapHeaderValidator soapHeaderValidator =
            new HttpSoapHeaderValidator();
        registerValidator(BindingInput.class,
                          SOAPConstants.Q_ELEM_SOAP_HEADER,
                          soapHeaderValidator);
        registerValidator(BindingOutput.class,
                          SOAPConstants.Q_ELEM_SOAP_HEADER,
                          soapHeaderValidator);

        HttpSoapBodyValidator soapBodyValidator =
            new HttpSoapBodyValidator();
        registerValidator(BindingInput.class,
                          SOAPConstants.Q_ELEM_SOAP_BODY,
                          soapBodyValidator);
        registerValidator(BindingOutput.class,
                          SOAPConstants.Q_ELEM_SOAP_BODY,
                          soapBodyValidator);
        registerValidator(MIMEPart.class,
                          SOAPConstants.Q_ELEM_SOAP_BODY,
                          soapBodyValidator);

        HttpSoapFaultValidator soapFaultValidator =
            new HttpSoapFaultValidator();
        registerValidator(BindingFault.class,
                          SOAPConstants.Q_ELEM_SOAP_FAULT,
                          soapFaultValidator);

        HttpSoapOperationValidator soapOperationValidator =
            new HttpSoapOperationValidator();
        registerValidator(BindingOperation.class,
                          SOAPConstants.Q_ELEM_SOAP_OPERATION,
                          soapOperationValidator);
        
        // Attach interested validators as observers to other validators
        soapHeaderValidator.attachObserver(soapBodyValidator);
        soapBodyValidator.attachObserver(soapHeaderValidator);
        soapHeaderValidator.getHeaderFaultValidator().attachObserver(soapBodyValidator);
        soapBodyValidator.attachObserver(soapHeaderValidator.getHeaderFaultValidator());
    }

    protected void registerHttpValidators() {
        HttpBindingValidator httpBindingValidator =
           new HttpBindingValidator();
        registerValidator(Binding.class,
                          HTTPConstants.Q_ELEM_HTTP_BINDING,
                          httpBindingValidator);
        
        HttpOperationValidator httpOperationValidator =
           new HttpOperationValidator();
        registerValidator(BindingOperation.class,
                          HTTPConstants.Q_ELEM_HTTP_OPERATION,
                          httpOperationValidator);
        
        HttpAddressValidator httpAddressValidator =
           new HttpAddressValidator(mRuntimeConfig, mResolveTokens);
        registerValidator(Port.class,
                          HTTPConstants.Q_ELEM_HTTP_ADDRESS,
                          httpAddressValidator);
    }

    public void registerValidator(Class parentType,
                                  QName elementType,
                                  Validator validator) {
        Map innerValidatorReg = (Map)mValidatorReg.get(parentType);

        if (innerValidatorReg == null) {
            innerValidatorReg = new HashMap();
            mValidatorReg.put(parentType, innerValidatorReg);
        }
        
        innerValidatorReg.put(elementType, validator);
    }


    public Validator queryValidator(Class parentType,
                                    QName elementType) {

        Map innerValidatorReg = (Map)mValidatorReg.get(parentType);
        Validator validator = null;
        
        if (innerValidatorReg != null) {
            validator = (Validator)innerValidatorReg.get(elementType);
        }

//         if (validator == null) {
//             throw new WSDLException(WSDLException.CONFIGURATION_ERROR,
//                                     "No Validator found " +
//                                     "to validate a '" + elementType +
//                                     "' element in the context of a '" +
//                                     parentType.getName() + "'.");
//         }

        return validator;
    }   
}
