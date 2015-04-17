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
 * @(#)HttpSoapEndpoint.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityManager;
import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfig;
import com.sun.jbi.httpsoapbc.security.http.impl.BasicAuthenticator;
import com.sun.jbi.httpsoapbc.security.impl.CredentialValidatorManager;
import com.sun.jbi.internationalization.Messages;
import com.sun.xml.ws.api.WSService;

import java.io.File;
import java.lang.reflect.Method;
import java.net.InetAddress;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;
import java.util.Set;

import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.jbi.JBIException;
import javax.security.auth.Subject;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.extensions.mime.MIMEPart;
import javax.wsdl.extensions.mime.MIMEMultipartRelated;
import javax.wsdl.Output;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.soap.SOAPAddress;
import javax.wsdl.extensions.soap.SOAPBinding;
import javax.wsdl.extensions.soap.SOAPBody;
import javax.wsdl.extensions.soap.SOAPHeader;
import javax.wsdl.extensions.soap.SOAPOperation;
import javax.xml.soap.SOAPMessage;
import javax.xml.ws.BindingProvider;
import javax.xml.ws.Dispatch;
import javax.xml.ws.Service;
import javax.xml.namespace.QName;



public class HttpSoapEndpoint extends AbstractEndpoint {

    protected static final Messages mMessages =
        Messages.getMessages(HttpSoapBindingDeployer.class);
    private transient Logger mLogger;
    protected Service mService;
    protected Subject mSubject;
    //protected Dispatch mDispatch;
    
    /*protected ThreadLocal<Dispatch<SOAPMessage>> dispatches = new ThreadLocal<Dispatch<SOAPMessage>> () {
        @Override
        protected Dispatch<SOAPMessage> initialValue(){
            return createDispatch();
        }
    };*/



    public HttpSoapEndpoint(Definition def,
                            QName serviceName,
                            String endpointName,
                            QName interfaceName, 
                            boolean isProvider,
                            HttpClientConnectionProperties clientConnProps,
                            boolean propagateSoapHeader,
                            String wsdlPath,
                            EndpointSecurityConfig securityConfig,
                            String serviceUnitID,
                            String serviceUnitRootPath,
                            CredentialValidatorManager cvm) {
        super(def, serviceName, endpointName, interfaceName, isProvider, clientConnProps, propagateSoapHeader, wsdlPath, securityConfig, serviceUnitID, serviceUnitRootPath, cvm);
        mLogger = mMessages.getLogger(getClass());
    }

    public void init() throws Exception {
        
        // If it has a soap binding, we need a soap address.  soap:address
        // should be a valid HTTP URL when we reach here. 
        String location = getEndpointUrl(getPort());
        
        // if HTTP BC is providing the service, i.e. a consumer of the endpoint, 
        // only "localhost" is allowed as the host in the URL location
        if (mIsInbound) {
            String hostname = new URL(location).getHost();
            if (!InetAddress.getLocalHost().getCanonicalHostName().equalsIgnoreCase(hostname) && !mValidHostnames.containsKey(hostname)) {
                throw new Exception(mMessages.getString("HTTPBC-E00298.Invalid_host_name", new Object[] { hostname, location, mWsdlPath }));
            }
        }
        

        ////////////////////////////////////////////////////////
        // GSR changed for Java EE Service Engine
        // As instructed by Jerry Waldorf.
        ////////////////////////////////////////////////////////
        if("REPLACE_WITH_ACTUAL_URL".equals(location)) {
            return;
        }

        setEndpointUrl(new URL(location));
        
        initSecuritySupport();
        
        // get the operation name from the wsdl, for now assume only 1 operation
        // but need to refactor this code so that a endpointbean is created for
        // each operation, or refactor endpointbean so that it can handle multiple
        // operations per endpoint
        BindingOperation[] bos = null;
        Definition currentDef = null;

        bos = getOperations(getServiceDescriptor(), getServiceName().toString(), getEndpointName());
        currentDef = getServiceDescriptor();
        if (bos == null) {
            String exMsg = mMessages.getString("HTTPBC-E00353.Endpoint_no_operation", new Object[] {getEndpointName()});
            throw new Exception(exMsg);
        }

        LinkedHashMap operationNameToMeta = new LinkedHashMap();
        for (int operationCount = 0; operationCount < bos.length; operationCount++) {
            OperationMetaData meta = new OperationMetaData();

            BindingOperation bo = bos[operationCount];
            String operationName = bo.getName();

            meta.setOperationName(operationName);

            // get the soap action for the operation
            String soapAction = getSoapAction(bo);
            if (soapAction == null) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "SOAP action is not set in the wsdl for operation " + operationName);
                }
                meta.setSoapActionURL("");
            } else {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "SOAP action is: " + soapAction);
                }
                meta.setSoapActionURL(soapAction);
            }

            // find out the input/out messages for this operation
            Input inMsg = bo.getOperation().getInput();
            if (inMsg != null) {
                if (inMsg.getMessage() == null) {
                    String exMsg = mMessages.getString("HTTPBC-E00354.Operation_no_message", new Object[] {inMsg.getName(), bo.getOperation().getName(), getEndpointName()});
                    throw new Exception(exMsg);
                }

                QName n = inMsg.getMessage().getQName();
                meta.setInMsgName(inMsg.getName());
                meta.setInMsgQName(n);
                meta.setInputMessage(inMsg.getMessage());
            }
            Output outMsg = bo.getOperation().getOutput();
            if (outMsg != null) {
                if (outMsg.getMessage() == null) {
                    String exMsg = mMessages.getString("HTTPBC-E00354.Operation_no_message", new Object[] {outMsg.getName(), bo.getOperation().getName(), getEndpointName()});
                    throw new Exception(exMsg);
                }

                QName n = outMsg.getMessage().getQName();
                meta.setOutMsgName(outMsg.getName());
                meta.setOutMsgQName(n);
                meta.setOutputMessage(outMsg.getMessage());
            }
            BindingInput inputBinding = bo.getBindingInput();
            meta.setBindingInput(inputBinding);
            if (inputBinding != null) {
                meta.setInputSoapBody(getSoapBody(inputBinding));
                meta.setInputSOAPMimeContents(getInputMimeParts(inputBinding));
            }
            BindingOutput outputBinding = bo.getBindingOutput();
            meta.setBindingOutput(outputBinding);
            if (outputBinding != null) {
                meta.setOutputSoapBody(getSoapBody(outputBinding));
                meta.setOutputSOAPMimeContents(getOutputMimeParts(outputBinding));
            }

            // soap headers for in/out
            if (inputBinding != null) {
                meta.setInputSoapHeaders(getSoapHeaders(inputBinding));
                // TODO: add mime support for headers
            }
            if (outputBinding != null) {
                meta.setOutputSoapHeaders(getSoapHeaders(outputBinding));
                // TODO: add mime support for headers
            }

            //Check soap:operation first then soap:binding
            boolean isDocument = true;
            String style = getSoapOpStyle(bo);
            if(style != null) {
                isDocument = style.equalsIgnoreCase("rpc") ? false:true;
            } else {
                Binding binding = null;
                binding = getBinding(getServiceDescriptor(), getServiceName().toString(), getEndpointName());
                if (binding == null) {
                    String exMsg = mMessages.getString("HTTPBC-E00355.Endpoint_no_binding", new Object[] {getEndpointName()});
                    throw new Exception(exMsg);
                }
                isDocument = isDocumentMode(binding);
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "isDocumentMode " + isDocument
                        + " for endpoint " + getEndpointName()
                        + " operation " + operationName);
            }
            meta.setIsDocumentMode(isDocument);
            
         // find out the faults this operation may throw
            Map m = bo.getOperation().getFaults();
            if (m != null) {
                meta.setFaults(m);
            }

            meta.setFullDefinition(currentDef);

            if (bo.getOperation().getStyle() == null) {
                String exMsg = mMessages.getString("HTTPBC-E00356.Operation_no_mep", new Object[] {operationName});
                throw new Exception(exMsg);
            } else {
                String mep = determineMEP(bo);
                meta.setMessageExchangePattern(mep);
            }
            Set partNames = getInputPartNames(meta);
            meta.setCachedInputPartNames(partNames);

            operationNameToMeta.put(operationName, meta);
        }
        setOperationNameToMetaData(operationNameToMeta);

        // Determine which operations have unique soap actions defined suitable for identification
        Set duplicates = getDuplicateSoapActions(operationNameToMeta);

        Collection metas = operationNameToMeta.values();
        Iterator metasIter = metas.iterator();
        while (metasIter.hasNext()) {
            OperationMetaData currMeta = (OperationMetaData) metasIter.next();
            String currSoapAction = currMeta.getSoapActionURL();
            String currOperation = currMeta.getOperationName();
            if (duplicates.contains(currSoapAction)) {
                // don't rely on soap action if they are not unique
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "soap actions for operation " + currOperation
                            + " are not unique (" + currSoapAction
                            + "), disabling use of soap action as hints");
                }
                currMeta.setUseSoapAction(false);
            } else {
                // use soap action if they are unique within a binding
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "using soap action  (" + currSoapAction + ") as hints for operation " + currOperation);
                }
                currMeta.setUseSoapAction(true);
            }

            // Determine whether the message signatures are unique within a binding

            // If document mode, for each entry, compare to all other operations
            boolean isUnique = true;
            if (currMeta.isDocumentMode()) {
                Iterator secondMetasIter = metas.iterator();
                while (secondMetasIter.hasNext()) {
                    OperationMetaData compareMeta = (OperationMetaData) secondMetasIter.next();
                    if (compareMeta != currMeta) {
                        Set compare = compareMeta.getCachedInputPartNames();
                        Set current = currMeta.getCachedInputPartNames();
                        if (compare.size() == current.size() && compare.containsAll(current)) {
                            // Message signature is not unique
                            isUnique = false;
                            if (mLogger.isLoggable(Level.FINE)) {
                                mLogger.log(Level.FINE, "The message signature for operation " + currOperation
                                        + " is not unique within the binding and will not be used as a factor to resolve operations for incoming messages");
                            }
                            break;
                        }
                    }
                }
            }
            currMeta.setUseMsgAsID(isUnique);
        }
    }

    public void activate(ComponentContext context) throws JBIException {

        if (!isInbound()) {
            // For outbound endpoint (not serviced by JBI) we are the proxy "provider" and activate them 
            try {
                QName fullServiceName = getServiceName();
                String endpointName = getEndpointName();
                // Check if this endpoint is service by JBI
                ServiceEndpoint endpointReference = 
                    context.activateEndpoint(fullServiceName, endpointName);
                setEndpointReference(endpointReference);

		// Use packaged WSDL to create the Service object
		File f = getOriginalWSDL();
		URL wsdlURL = f.toURI().toURL();
		
		mService = createService(wsdlURL, fullServiceName);                

                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Endpoint " + getUniqueName() + " activated");
                }        
            } catch(Exception me) {
                String text = mMessages.getString("HTTPBC-E00301.Endpoint_activate_failed",
                            new Object[] {getUniqueName(), me.getMessage()});
                mLogger.log(Level.SEVERE, text, me);
                throw new JBIException(text, me);
            }  
        }
    }

    public void deactivate(ComponentContext context) throws JBIException {

        if (!isInbound()) {
            ServiceEndpoint endpointReference = getEndpointReference();
            if (endpointReference != null) {
                context.deactivateEndpoint(endpointReference);
            }

            if(securityEnabled && basicAuthCredentials != null) {
                BasicAuthenticator.unregisterEndPointCredentials(this, basicAuthCredentials);
            }        
            
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Endpoint " + getUniqueName() + " activated");
            }
        }
    }

    public String getSoapAction(BindingOperation bo) {
        if (bo == null) {
            return null;
        }
        List extElems = bo.getExtensibilityElements();
            
        //<soap:operation soapAction="http://www.webserviceX.NET/GetQuote" style="document"/>
        String soapAction = null;
        Iterator extIter = extElems == null ? null : extElems.iterator();
        while (extIter != null && extIter.hasNext()) {
            ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
            if (SOAPOperation.class.isInstance(ee)) {
                SOAPOperation oper = (SOAPOperation) ee;
                soapAction = oper.getSoapActionURI();
            }
                
        }
        return soapAction;
    }
        
    public String getSoapOpStyle(BindingOperation bo) {
        if (bo == null) {
            return null;
        }
        List extElems = bo.getExtensibilityElements();
            
        //<soap:operation soapAction="http://www.webserviceX.NET/GetQuote" style="document"/>
        String soapOpStyle = null;
        Iterator extIter = extElems == null ? null : extElems.iterator();
        while (extIter != null && extIter.hasNext()) {
            ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
            if (SOAPOperation.class.isInstance(ee)) {
                SOAPOperation oper = (SOAPOperation) ee;
                soapOpStyle = oper.getStyle();
            }
                
        }
        return soapOpStyle;
    }
        
    public SOAPBody getSoapBody(BindingInput bi) {
        // assert bi != null
        if (bi == null) {
            throw new IllegalArgumentException(mMessages.getString("HTTPBC-E00357.BindingInput_null"));
        }
        return getSoapBody0(bi.getExtensibilityElements());
    }
        
    protected SOAPBody getSoapBody0(List extElems) {
        //soap:body
        Iterator extIter = extElems == null ? null : extElems.iterator();
        while (extIter != null && extIter.hasNext()) {
            ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
            if (SOAPBody.class.isInstance(ee)) {    // no mime extensions
                SOAPBody body = (SOAPBody) ee;
                return body;
            } else {
                if (MIMEMultipartRelated.class.isInstance(ee)) {
                    MIMEMultipartRelated mimeMultipartsRelated = (MIMEMultipartRelated) ee;
                    List mimeParts = mimeMultipartsRelated.getMIMEParts();
                    
                    Iterator mimeIter = mimeParts.iterator();
                    while (mimeIter.hasNext()) {
                        MIMEPart mimePart = (MIMEPart) mimeIter.next();
                        List extensions = mimePart.getExtensibilityElements();
                        
                        Iterator mimePartIter = extensions.iterator();
                        while (mimePartIter.hasNext()) {
                            ee = (ExtensibilityElement) mimePartIter.next();
                            // only one soap:body allowed in the mime:part extension
                            // and by this point, we should have done the validation already
                            if (SOAPBody.class.isInstance (ee)) {
                                SOAPBody body = (SOAPBody) ee;
                                return body;
                            }
                        }
                    }
                }
            }   
        }
        return null;
    }
        
    public SOAPBody getSoapBody(BindingOutput bo) {
        // assert bi != null
        if (bo == null) {
            throw new IllegalArgumentException(mMessages.getString("HTTPBC-E00358.BindingOutput_null"));
        }
        return getSoapBody0(bo.getExtensibilityElements());
    }
        
    public List getSoapHeaders(BindingInput bi) {
        if (bi == null) {
            throw new IllegalArgumentException(mMessages.getString("HTTPBC-E00358.BindingOutput_null"));
        }
        return getSoapHeaders0(bi.getExtensibilityElements());
    }
        
    public List getSoapHeaders(BindingOutput bo) {
        if (bo == null) {
            throw new IllegalArgumentException(mMessages.getString("HTTPBC-E00358.BindingOutput_null"));
        }
        return getSoapHeaders0(bo.getExtensibilityElements());
    }
        
    public List getSoapHeaders0(List extElems) {
        //soap:header
        List headers = new ArrayList();
        Iterator extIter = extElems == null ? null : extElems.iterator();
        while (extIter != null && extIter.hasNext()) {
            ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
            if (SOAPHeader.class.isInstance(ee)) {
                SOAPHeader header = (SOAPHeader) ee;  
                headers.add(header);
            } else {  
            	// soap:header may be embedded in a mime extension
            	// good news is that by this point, all the mime extension validations would have
            	// been done already(at deployment time), so we are just looking for soap:header extensions here.
                if (MIMEMultipartRelated.class.isInstance(ee)) {
                    MIMEMultipartRelated mimeMultipartsRelated = (MIMEMultipartRelated) ee;
                    List mimeParts = mimeMultipartsRelated.getMIMEParts();
                    
                    Iterator mimeExtIter = mimeParts.iterator();
                    while (mimeExtIter.hasNext()) {
                        MIMEPart mimePart = (MIMEPart) mimeExtIter.next();
                        List extensions = mimePart.getExtensibilityElements();
                        Iterator mimePartIter = extensions.iterator();
                        while (mimePartIter.hasNext()) {
                            ee = (ExtensibilityElement) mimePartIter.next();
                            if (SOAPHeader.class.isInstance (ee)) {
                                SOAPHeader header = (SOAPHeader) ee;
                                headers.add(header);
                            }
                        }
                    }
                }
            }   
        }
        return headers;
    }
        
    public String getEndpointUrl(Port port) {

        String location = null;
        if (port != null) {
            List extElems = port.getExtensibilityElements();
            //soap:address
                
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext()) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (SOAPAddress.class.isInstance(ee)) {
                    SOAPAddress addr = (SOAPAddress) ee;
                    location = addr.getLocationURI();
                }
                    
            }
        }
        return location;
    }

    public boolean isDocumentMode(Binding binding) {
        //soap:binding
        //default for soap:binding style is true
        
        List extElems = binding.getExtensibilityElements();
        Iterator extIter = extElems == null ? null : extElems.iterator();
        while (extIter != null && extIter.hasNext()) {
            ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
            if (SOAPBinding.class.isInstance(ee)) {
                SOAPBinding soapBinding = (SOAPBinding) ee;
                return (soapBinding.getStyle() == null) ? true : "document".equalsIgnoreCase(soapBinding.getStyle()); // NOI18N
            }
                
        }
        return true;
    }

    /**
     * Determine whether the operation metadata 
     * for the given operations contains non-unique soap actions
     *
     * @return the list of dupplicates
     */    
    protected Set getDuplicateSoapActions(Map map) {
        Collection c = map.values();
        Iterator iter = c.iterator();
        Set seen = new HashSet();
        Set duplicates = new HashSet();
        while (iter.hasNext()) {
            OperationMetaData meta = (OperationMetaData) iter.next();
            String s = meta.getSoapActionURL();
            if (s != null) {
                if (seen.contains(s)) {
                    duplicates.add(s);
                } else {
                    seen.add(s);
                }
            }
        }
        return duplicates;
    }
    
    
    /**
     * Get the input message part names
     * @param meta the operation definition
     * @return part names
     */
    protected Set getInputPartNames(OperationMetaData meta) {
        Set set = new HashSet();
        
        Message msg = meta.getInputMessage();
        
        if (msg != null) {
            SOAPBody bodyBinding = meta.getInputSoapBody();
            // Should we validate here the bodyBinidng object?  Don't do it for now.
            List bodyParts = null;
            if (bodyBinding != null) {
                bodyParts = bodyBinding.getParts();
            }
            Iterator bodyIterator = null;
            if (bodyParts != null && bodyParts.size() > 0) {
                // since soap body binding specifies parts, we use only the parts that are in the binding
                bodyIterator = bodyParts.iterator();
            } else {
                // if no parts were specified in the SOAP binding section, we use all the message parts
                // currently doesn't care about order of parts
                Map msgParts = msg.getParts();
                if (msgParts != null) {
                    bodyIterator = msgParts.keySet().iterator();
                }
            }
            // construct a set of expected names
            while (bodyIterator != null && bodyIterator.hasNext()) {
                String bodyPart = (String) bodyIterator.next();
                Part part = msg.getPart(bodyPart);
                if (part == null) {
                    // assert failed
                    String err = mMessages.getString("HTTPBC-E00352.Part_not_in_message", new Object[] {bodyPart, msg.getQName().toString()});
                    throw new IllegalStateException(err);
                }
                QName typeQName = part.getTypeName();
                QName elemQName = part.getElementName();
                
                if (typeQName != null) {
                    // it uses type, so the root node name is the part name
                    set.add(new QName(part.getName()));
                } else if (elemQName != null) {
                    // it uses element, so the root node name is the element name
                    set.add(elemQName);
                } else {
                    mLogger.log(Level.SEVERE, "HTTPBC-E00351.Part_neither_element_or_type", bodyPart);
                }
            }
        }
        
        return set;
    }
    
    protected Service createService(URL wsdlURL, QName fullServiceName) throws Exception {
    	Service service = null;
        // let's see if com.sun.xml.ws.api.WSService is in webservice-rt.jar in GF
        Class wsServiceClass = null;
        Class initParamsClass = null;
        try {
            // try to load the class using the default boostrap classloader
            wsServiceClass = Class.forName("com.sun.xml.ws.api.WSService", true, Thread.currentThread().getContextClassLoader());  // load from the thread context classloader
            initParamsClass = Class.forName("com.sun.xml.ws.api.WSService$InitParams", true, Thread.currentThread().getContextClassLoader());
        } catch (Exception e) {
            // Failed to load either class. Metro updates required for client side WSIT features are not available
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, mMessages.getString("HTTPBC-W00364.Metro_updates_not_available"));
            }  
        }
        if (wsServiceClass != null && initParamsClass != null) {
            try {
                Object initParamsInstance = initParamsClass.newInstance();
                //Object wsServiceInstance = wsServiceClass.newInstance();
                Class[] args;
                Object[] params;
                Method methodToInvoke;
                
                // find the setContainer method in com.sun.xml.ws.api.WSService.InitParams
                args = new Class[] {com.sun.xml.ws.api.server.Container.class};
                methodToInvoke = initParamsClass.getMethod("setContainer", args);
                if (methodToInvoke != null) {
                    params = new Object[] {new HttpSpiContainerImpl(this)};
                    methodToInvoke.invoke(initParamsInstance, params);
                } else {
                    throw new Exception(mMessages.getString("HTTPBC-W00365.Failed_to_find_method_in_InitParams"));
                }
                
                // find the create(URL, QName, InitParams) method in com.sun.xml.ws.api.WSService
                args = new Class[] { URL.class, QName.class, initParamsClass};
                methodToInvoke = wsServiceClass.getMethod("create", args);
                if (methodToInvoke != null) {
                    params = new Object[] { wsdlURL, fullServiceName, initParamsInstance};
                    service = (Service) methodToInvoke.invoke(null, params);  // static method
                } else {
                    throw new Exception(mMessages.getString("HTTPBC-W00366.Failed_to_find_method_in_WSService"));
                }
            } catch (Exception e) {
                throw new Exception(mMessages.getString("HTTPBC-E00363.Cannot_create_service_object", e.getMessage()), e);
            }
        } else {
            service = Service.create(wsdlURL, fullServiceName);
        }
        
        return service;
    }
    
    Dispatch<SOAPMessage> createDispatch() {
	QName fullServiceName = getServiceName();
	String endpointName = getEndpointName();
	QName qualifiedEndpointName =
	    new QName(fullServiceName.getNamespaceURI(), endpointName);
	Dispatch<SOAPMessage> dispatch =
	    mService.createDispatch(qualifiedEndpointName,
                                    SOAPMessage.class,
                                    javax.xml.ws.Service.Mode.MESSAGE,
                                    new javax.xml.ws.WebServiceFeature[] {
                                        new javax.xml.ws.RespectBindingFeature(true),
                                        new javax.xml.ws.soap.AddressingFeature(true)
                                    });	
	return dispatch;
    }
    
    public Dispatch<SOAPMessage> createDispatch(String soapActionURL) {    
	// add the soap action if any
	/*if (mDispatch == null) {
	    mDispatch = createDispatch();
	}*/
        Dispatch<SOAPMessage> dispatch = DispatchPool.instance().retain(this);

        if (soapActionURL == null) {  // TODO: check me later
            soapActionURL = "";
        }
    	dispatch.getRequestContext().put(BindingProvider.SOAPACTION_USE_PROPERTY, true);
        dispatch.getRequestContext().put(BindingProvider.SOAPACTION_URI_PROPERTY, soapActionURL);
       
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Invoking an external web service using URL: " + getEndpointUrl().toString());
        }
        dispatch.getRequestContext().put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY, getEndpointUrl().toString());
	
	return dispatch;
    }
    
    public Dispatch<SOAPMessage> createDispatch (String soapActionURL, String aDynamicUrl) {
        if (soapActionURL == null) { // TODO: check me later
            soapActionURL = "";
        }
        /*if (mDispatch == null)
            mDispatch = createDispatch();*/

        Dispatch<SOAPMessage> dispatch = DispatchPool.instance().retain(this);
    	dispatch.getRequestContext().put(BindingProvider.SOAPACTION_USE_PROPERTY, true);
        dispatch.getRequestContext().put(BindingProvider.SOAPACTION_URI_PROPERTY, soapActionURL);
	
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Invoking an external web service using URL: " + aDynamicUrl);
        }
           
        dispatch.getRequestContext().put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY, aDynamicUrl);
	
	return dispatch;
    }

    public void releaseDispatch(Dispatch<SOAPMessage> dispatch) {
        DispatchPool.instance().release(this, dispatch);
    }

    public EndpointSecurityManager getEndpointSecurityManager() {
    	return BasicAuthenticator.getEndpointSecurityManager();
    }


    public void setSubject(Subject subject) {
        mSubject = subject;
    }

    public Subject getSubject() {
        return mSubject;
    }
}
