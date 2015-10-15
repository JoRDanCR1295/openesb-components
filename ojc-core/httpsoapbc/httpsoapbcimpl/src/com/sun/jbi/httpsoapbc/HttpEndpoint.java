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
 * @(#)HttpEndpoint.java 
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

import java.net.URL;
import java.net.InetAddress;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;

import java.util.concurrent.ConcurrentHashMap;
import javax.jbi.component.ComponentContext;
import javax.jbi.JBIException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.security.auth.Subject;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.http.HTTPAddress;
import javax.wsdl.extensions.http.HTTPBinding;
import javax.wsdl.extensions.http.HTTPOperation;
import javax.wsdl.extensions.http.HTTPUrlEncoded;
import javax.wsdl.extensions.http.HTTPUrlReplacement;
import javax.wsdl.extensions.mime.MIMEContent;
import javax.wsdl.Input;
import javax.wsdl.Output;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.xml.namespace.QName;
import javax.xml.ws.BindingProvider;
import javax.xml.ws.Dispatch;
import javax.xml.ws.Service;
import javax.xml.ws.Service.Mode;

public class HttpEndpoint extends AbstractEndpoint {

    // HTTP GET&POST
    public static final String HTTP_BINDING_VERB_GET = "GET";                      // NOI18N
    public static final String HTTP_BINDING_VERB_POST = "POST";                    // NOI18N
    private static final Messages mMessages =
            Messages.getMessages(HttpSoapBindingDeployer.class);
    private transient Logger mLogger;
    private String mHttpBindingVerb;
    private Subject mSubject;
    private Service mService;
    private QName mPortName;
    //private Dispatch mDispatch;
    private ConcurrentHashMap<Class, Dispatch> dispatchCache;

    public HttpEndpoint(Definition def,
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
        mPortName = new QName(getServiceName().getNamespaceURI(), getEndpointName());
        dispatchCache = new ConcurrentHashMap<Class, Dispatch>();
    }

    public void init() throws Exception {

        // If it has a http binding, we need a http address.
        String adrLocation = getHttpAddressLocation(getPort());

        // if HTTP BC is providing the service, i.e. a consumer of the endpoint, 
        // only "localhost" is allowed as the host in the URL location
        if (mIsInbound) {
            String hostname = new URL(adrLocation).getHost();
            if (!InetAddress.getLocalHost().getCanonicalHostName().equalsIgnoreCase(hostname) && !mValidHostnames.containsKey(hostname)) {
                throw new Exception(mMessages.getString("HTTPBC-E00298.Invalid_host_name", new Object[]{hostname, adrLocation, mWsdlPath}));
            }
        }

        URL url = new URL(adrLocation);

        setEndpointUrl(url);

        initSecuritySupport();

        // get the operation name from the wsdl, for now assume only 1 operation
        // but need to refactor this code so that a endpointbean is created for
        // each operation, or refactor endpointbean so that it can handle multiple
        // operations per endpoint
        Binding binding = null;
        BindingOperation[] bos = null;
        Definition currentDef = getServiceDescriptor();
        binding = getBinding(getServiceDescriptor(), getServiceName().toString(), getEndpointName());
        bos = getOperations(getServiceDescriptor(), getServiceName().toString(), getEndpointName());

        if (binding == null) {
            String exMsg = mMessages.getString("HttpSoapBindingDeployer.Missing_wsdl_binding_for_endpoint", new Object[]{getEndpointName()});
            throw new Exception(exMsg);
        }
        HTTPBinding httpBinding = getHTTPBinding(binding);
        String bindingVerb = httpBinding.getVerb();
        setHttpBindingVerb(bindingVerb);

        if (bos == null) {
            String exMsg = mMessages.getString("HttpSoapBindingDeployer.Missing_operation_for_endpoint_during_deployment", new Object[]{getEndpointName()});
            throw new Exception(exMsg);
        }
        LinkedHashMap operationNameToMeta = new LinkedHashMap();
        for (int operationCount = 0; operationCount < bos.length; operationCount++) {
            OperationMetaData meta = new OperationMetaData();

            BindingOperation bo = bos[operationCount];
            String operationName = bo.getName();

            meta.setOperationName(operationName);

            // get the operationLocation for the operation
            String opLocation = getHttpOperationLocation(bo);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "The location attribute on <http:operation> element is " + opLocation);
            }
            meta.setHttpOperationLocation(opLocation);

            // find out the input/out messages for this operation
            Input opInput = bo.getOperation().getInput();
            if (opInput != null) {
                if (opInput.getMessage() == null) {
                    String exMsg = mMessages.getString("HttpSoapBindingDeployer.Message_could_not_be_found_for_operation_input", new Object[]{opInput.getName(), bo.getOperation().getName(), getEndpointName()});
                    throw new Exception(exMsg);
                }

                QName n = opInput.getMessage().getQName();
                meta.setInMsgName(opInput.getName());
                meta.setInMsgQName(n);
                meta.setInputMessage(opInput.getMessage());

                List partList = opInput.getMessage().getOrderedParts(null);
                List partNameList = new ArrayList();
                for (int i = 0; i < partList.size(); i++) {
                    partNameList.add(((Part) partList.get(i)).getName());
                }
                meta.setCachedInputPartNameList(partNameList);
            }
            Output opOutput = bo.getOperation().getOutput();
            if (opOutput != null) {
                if (opOutput.getMessage() == null) {
                    String exMsg = mMessages.getString("HttpSoapBindingDeployer.Message_could_not_be_found_for_operation_output", new Object[]{opOutput.getName(), bo.getOperation().getName(), getEndpointName()});
                    throw new Exception(exMsg);
                }

                QName n = opOutput.getMessage().getQName();
                meta.setOutMsgName(opOutput.getName());
                meta.setOutMsgQName(n);
                meta.setOutputMessage(opOutput.getMessage());
            }
            BindingInput bindingInput = bo.getBindingInput();
            meta.setBindingInput(bindingInput);
            if (bindingInput != null) {
                meta.setHttpUrlEncoding(getHttpUrlEncoding(bindingInput));
                meta.setInputMimeContents(getMimeContent(bindingInput));
            }
            BindingOutput bindingOutput = bo.getBindingOutput();
            meta.setBindingOutput(bindingOutput);
            if (bindingOutput != null) {
                meta.setOutputMimeContents(getMimeContent(bindingOutput));
            }

            // find out the faults this operation may throw
            Map m = bo.getOperation().getFaults();
            if (m != null) {
                meta.setFaults(m);
            }

            meta.setFullDefinition(currentDef);

            String mep = determineMEP(bo);
            meta.setMessageExchangePattern(mep);

            operationNameToMeta.put(operationName, meta);
        }
        setOperationNameToMetaData(operationNameToMeta);

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
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE,
                            "Activated outbound endpoint " +
                            getUniqueName());
                }

                mService = Service.create(fullServiceName);
            } catch (JBIException me) {
                String text = mMessages.getString("HTTPBC-E00301.Endpoint_activate_failed",
                        new Object[]{getUniqueName(), me.getMessage()});
                mLogger.log(Level.SEVERE, text, me);
            }
        }
    }

    public void deactivate(ComponentContext context) throws JBIException {

        if (!isInbound()) {
            ServiceEndpoint endpointReference = getEndpointReference();
            if (endpointReference != null) {
                context.deactivateEndpoint(endpointReference);
            }

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Activated outbound endpoint " + getUniqueName());
            }
        }
    }

    public String getHttpBindingVerb() {
        return mHttpBindingVerb;
    }

    public void setHttpBindingVerb(String httpBindingVerb) {
        mHttpBindingVerb = httpBindingVerb;
    }

    // HTTP GET&POST BINDING
    public String getHttpAddressLocation(Port port) {
        String location = null;
        if (port != null) {
            List extElems = port.getExtensibilityElements();
            //<service .. >
            //  <port ..>
            //    <http:address location="http..."/>
            //  </port..>
            //  ...
            //</service>

            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext()) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (HTTPAddress.class.isInstance(ee)) {
                    HTTPAddress addr = (HTTPAddress) ee;
                    location = addr.getLocationURI();
                    break;
                }
            }
        }
        return location;
    }

    public String getHttpOperationLocation(BindingOperation bo) {
        if (bo == null) {
            return null;
        }
        List extElems = bo.getExtensibilityElements();

        //<binding ..>
        //  <http:binding verb="GET"/>
        //  <operation ...>   // BindingOperation
        //     <http:operation location="o1"/>
        //     <input>
        //           <http:urlEncoded/>
        //     </input>
        //     <output>
        //           <mime:content type="image/gif"/>
        //           <mime:content type="image/jpeg"/>
        //     </output>
        //  </operation>
        //</binding>
        String opLoc = null;
        Iterator extIter = extElems == null ? null : extElems.iterator();
        while (extIter != null && extIter.hasNext()) {
            ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
            if (HTTPOperation.class.isInstance(ee)) {
                HTTPOperation oper = (HTTPOperation) ee;
                opLoc = oper.getLocationURI();
                break;
            }

        }
        return opLoc;
    }

    public String getHttpUrlEncoding(BindingInput bindingInput) {
        if (bindingInput == null) {
            return null;
        }
        List extElems = bindingInput.getExtensibilityElements();

        //<binding ..>
        //  <http:binding verb="GET"/>
        //  <operation ...>   // BindingOperation
        //     <http:operation location="o1"/>
        //     <input>
        //           <http:urlEncoded/>
        //     </input>
        //     <output>
        //           <mime:content type="image/gif"/>
        //           <mime:content type="image/jpeg"/>
        //     </output>
        //  </operation>
        //</binding>
        Iterator extIter = extElems == null ? null : extElems.iterator();
        while (extIter != null && extIter.hasNext()) {
            ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
            if (HTTPUrlEncoded.class.isInstance(ee)) {
                return OperationMetaData.HTTP_URL_ENCODING_ENCODED;
            }
            if (HTTPUrlReplacement.class.isInstance(ee)) {
                return OperationMetaData.HTTP_URL_ENCODING_REPLACEMENT;
            }
        }
        return OperationMetaData.HTTP_URL_ENCODING_UNSPECIFIED;
    }

    public List getMimeContent(BindingInput bindingInput) {
        List mcList = new ArrayList();
        if (bindingInput == null) {
            return mcList;
        }
        List extElems = bindingInput.getExtensibilityElements();

        //<binding ..>
        //  <http:binding verb="GET"/>
        //  <operation ...>   // BindingOperation
        //     <http:operation location="o1"/>
        //     <input>
        //           <http:urlEncoded/>
        //     </input>
        //     <output>
        //           <mime:content type="image/gif"/>
        //           <mime:content type="image/jpeg"/>
        //     </output>
        //  </operation>
        //</binding>
        Iterator extIter = extElems == null ? null : extElems.iterator();
        while (extIter != null && extIter.hasNext()) {
            ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
            if (MIMEContent.class.isInstance(ee)) {
                mcList.add(ee);
            }
        }
        return mcList;
    }

    public List getMimeContent(BindingOutput bindingOutput) {
        List mcList = new ArrayList();
        if (bindingOutput == null) {
            return mcList;
        }
        List extElems = bindingOutput.getExtensibilityElements();

        //<binding ..>
        //  <http:binding verb="GET"/>
        //  <operation ...>   // BindingOperation
        //     <http:operation location="o1"/>
        //     <input>
        //           <http:urlEncoded/>
        //     </input>
        //     <output>
        //           <mime:content type="image/gif"/>
        //           <mime:content type="image/jpeg"/>
        //     </output>
        //  </operation>
        //</binding>
        Iterator extIter = extElems == null ? null : extElems.iterator();
        while (extIter != null && extIter.hasNext()) {
            ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
            if (MIMEContent.class.isInstance(ee)) {
                mcList.add(ee);
            }
        }
        return mcList;
    }

    public HTTPBinding getHTTPBinding(Binding binding) {
        //http:binding
        HTTPBinding httpBinding = null;
        if (binding != null) {
            List extElems = binding.getExtensibilityElements();
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext()) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (HTTPBinding.class.isInstance(ee)) {
                    httpBinding = (HTTPBinding) ee;
                    break;
                }

            }
        }
        return httpBinding;
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

    public <T> Dispatch<T> createDispatch(String url, Class<T> type){
        Dispatch<T> d = dispatchCache.get(type);

        if (d == null) {
            mService.addPort(mPortName, javax.xml.ws.http.HTTPBinding.HTTP_BINDING, url);
            d = mService.createDispatch(mPortName, type, Mode.MESSAGE);
            dispatchCache.put(type, d);
        }
        
        d.getRequestContext().put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY, url);
        d.getRequestContext().put(javax.xml.ws.http.HTTPBinding.HTTP_BINDING, url);
        return d;
    }
}
