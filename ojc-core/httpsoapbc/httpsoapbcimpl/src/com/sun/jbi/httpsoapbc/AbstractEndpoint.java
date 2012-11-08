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
 * @(#)AbstractEndpoint.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.httpsoapbc.descriptors.HttpSoapHandler;
import com.sun.jbi.httpsoapbc.extensions.PolicyReference;
import com.sun.jbi.httpsoapbc.extensions.Policy;
import com.sun.jbi.httpsoapbc.security.api.CredentialValidator;
import com.sun.jbi.httpsoapbc.security.api.HttpBcSecurityException;
import com.sun.jbi.httpsoapbc.security.api.HTTPBasicAuthCredential;
import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfig;
import com.sun.jbi.httpsoapbc.security.util.impl.Base64Impl;
import com.sun.jbi.httpsoapbc.security.impl.AuthInfo;
import com.sun.jbi.httpsoapbc.security.impl.CredentialValidatorManager;
import com.sun.jbi.internationalization.Messages;
import com.sun.xml.ws.api.server.WSEndpoint;

import java.io.Serializable;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.nio.ByteBuffer;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.logging.Logger;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import javax.jbi.JBIException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.security.auth.Subject;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Service;
import javax.wsdl.Port;
import javax.wsdl.BindingOperation;
import javax.wsdl.OperationType;
import javax.wsdl.Binding;
import javax.wsdl.extensions.mime.MIMEContent;
import javax.wsdl.extensions.mime.MIMEPart;
import javax.wsdl.extensions.mime.MIMEMultipartRelated;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLWriter;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/** 
 * Base implementation of Endpoint interface
 */
public abstract class AbstractEndpoint
    implements Endpoint, Serializable {
    
    private static final Messages mMessages =
        Messages.getMessages(HttpSoapBindingDeployer.class);

    private static final String ENDPOINT_TYPE_INBOUND = StatusProviderHelper.CONSUMING_ID;
    private static final String ENDPOINT_TYPE_OUTBOUND = StatusProviderHelper.PROVISIONING_ID;
    
    private Logger mLogger;

    protected Definition mDefinition;
    protected Port mMatchedPort;
    
    protected File mOriginalWSDL;
    protected List mOriginalWSDLImports;
    protected List mImportedWSDLNamespaces;
    protected List mImportedXSDNamespaces;

    protected EndpointStatus mEndpointStatus;
    
    protected URL mEndpointUrl;
    protected ServiceEndpoint mServiceEndpoint;
    protected String mEndpointName;
    protected String mWsdlPath;
    protected QName mServiceName;
    protected QName mInterfaceName;  
    protected boolean mIsInbound;
    protected Map mOperationNameToMetaData;
    protected Map mImportedWSDLDefinitions;
    protected Map mImportedXSDDefinitions;
    protected WSEndpoint mWSEndpoint;
    protected Map<String, Definition> mImpotedURL2WSDL;
    protected Map<String, Element> mImportedURL2XSD;
    protected Map<String, String> mValidHostnames;

    protected String mServiceUnitID;
    protected String mServiceUnitRootPath;
    protected int maxConcurrencyLimit = -1;
    protected RedeliveryConfig mRedeliveryConfig;
    
    protected int pendingExchanges = 0;
    protected RequestThrottlingController throttlingController;
    protected CredentialValidatorManager mCredValidatorMgr;
    protected Policy mSecPolicy;    
    protected boolean mAuthenticateClient = false;        
    protected Base64Impl base64;
    protected boolean securityEnabled;
    protected HttpClientConnectionProperties mHttpClientConnProps;
    protected boolean mPropagateSoapHeader;
    protected HTTPBasicAuthCredential basicAuthCredentials;
    protected EndpointSecurityConfig mSecurityConfig;
    protected long mMessageIdCounter = 0;
    protected boolean mEnableWsdlQuery = true;
    
    protected List<HttpSoapHandler> mHandlers;
    protected List mHandlerLibPaths;
    
    public AbstractEndpoint(Definition def,
                            QName serviceName,
                            String endpointName,
                            QName interfaceName,
                            boolean isProvider,
                            HttpClientConnectionProperties httpClientConnProps,
                            boolean propagateSoapHeader,
                            String wsdlPath,
                            EndpointSecurityConfig securityConfig,
                            String serviceUnitID,
                            String serviceUnitRootPath,
                            CredentialValidatorManager cvm) {
        mLogger = Messages.getLogger(getClass());
        mDefinition = def;
        mServiceName = serviceName;
        mEndpointName =  QName.valueOf(endpointName).getLocalPart();
        mInterfaceName = interfaceName;
        mIsInbound = !isProvider;
        mHttpClientConnProps = httpClientConnProps;
        mPropagateSoapHeader = propagateSoapHeader;
        mWsdlPath = wsdlPath;
        mSecurityConfig = securityConfig;
        mOriginalWSDLImports = new ArrayList();
        mImportedWSDLNamespaces = new ArrayList();
        mImportedXSDNamespaces = new ArrayList();
        mImportedWSDLDefinitions = new HashMap();
        mImportedXSDDefinitions = new HashMap();
        mValidHostnames = new HashMap();
        mServiceUnitID = serviceUnitID;
        mServiceUnitRootPath = serviceUnitRootPath;
        mCredValidatorMgr = cvm;        
    }
    
    public void setEndpointStatus(EndpointStatus val) {
        mEndpointStatus = val;
    }

    public EndpointStatus getEndpointStatus() {
        return mEndpointStatus;
    }

    /**
     * Get the unique name of this endpoint instance
     */
    public String getUniqueName() {
        String serviceNamespaceURI = getServiceName().getNamespaceURI();
        String serviceName = getServiceName().getLocalPart();
        String endpointName = getEndpointName();
        return getUniqueName(serviceNamespaceURI, serviceName, endpointName, isInbound());
    }

    /**
     * Utility method to create the unique names with explicit arguments
     */
    public static String getUniqueName(String aServiceNamespaceURI,
                                       String aServiceName,
                                       String aEndpointName, 
                                       boolean isInbound) {
        String aEndpointType =
            isInbound ? ENDPOINT_TYPE_INBOUND : ENDPOINT_TYPE_OUTBOUND;
        return aServiceNamespaceURI + "," + aServiceName + "," + aEndpointName + "," + aEndpointType;
    }

    public WSEndpoint getWSEndpoint() {
        return mWSEndpoint;
    }

    public void setWSEndpoint(WSEndpoint wsEndpoint) {
        mWSEndpoint = wsEndpoint;
    }
    
    public Definition getServiceDescriptor() {
        return mDefinition;
    }

    public void setServiceDescriptor(Definition def) {
        mDefinition = def;
    }

    public Document getServiceDescriptorAsDocument() {
        try {
            WSDLFactory wsdlFactory = (WSDLFactory)WSDLFactory.newInstance();
            WSDLWriter writer = (WSDLWriter)wsdlFactory.newWSDLWriter();
            return writer.getDocument(getServiceDescriptor());
        } catch (Exception ex) {
            //Ignore on purpose.  We return null if this fails
        }
        return null;
    }

    public ByteBuffer getServiceDescriptorAsByteBuffer() {
        try {
            WSDLFactory wsdlFactory = (WSDLFactory)WSDLFactory.newInstance();
            WSDLWriter writer = (WSDLWriter)wsdlFactory.newWSDLWriter();
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            writer.writeWSDL(getServiceDescriptor(), baos);
            return ByteBuffer.wrap(baos.toByteArray());
        } catch (Exception ex) {
            // Ignore on purpose.  We return null if this fails.
        }
        return null;
    }

    public void setOriginalWSDL(File originalWSDL) {
        mOriginalWSDL = originalWSDL;
    }
    
    public File getOriginalWSDL() {
        return mOriginalWSDL;
    }
    
    public void addWSDLImport(URL anImport) {
        mOriginalWSDLImports.add(anImport);
    }
    
    public List getWSDLImports() {
        return mOriginalWSDLImports;
    }
    
    public void addImportedWSDLNamespace(String namespace) {
        mImportedWSDLNamespaces.add(namespace);
    }
    
    public List getImportedWSDLNamespaces() {
        return mImportedWSDLNamespaces;
    }
    
    public void addImportedXSDNamespace(String namespace) {
        mImportedXSDNamespaces.add(namespace);
    }
    
    public List getImportedXSDNamespaces() {
        return mImportedXSDNamespaces;
    }
    
//    public void addImportedWSDLDefinition(String namespace, Definition definition) {
//        mImportedWSDLDefinitions.put(namespace, definition);
//    }
//
//    
//    public Map getImportedWSDLDefinitions() {
//        return mImportedWSDLDefinitions;
//    }
//    
//    public void addImportedXSDSchema(String namespace, Element schemaElement) {
//        mImportedXSDDefinitions.put(namespace, schemaElement);
//    }
//    
//    public Map getImportedXSDSchemas() {
//        return mImportedXSDDefinitions;
//    }
//    
//    public Map <String, Definition> getImportedURL2WSDL() {
//    	return mImpotedURL2WSDL;
//    }
//    
//    public void setImportedURL2WSDL(Map<String, Definition> importedWSDLs) {
//    	mImpotedURL2WSDL = importedWSDLs;
//    }
//    
//    public Map <String, Element> getImportedURL2XSD() {
//    	return mImportedURL2XSD;
//    }
//    
//    public void setImportedURL2XSD(Map<String, Element> importedXSDs) {
//    	mImportedURL2XSD = importedXSDs;
//    }
    

    public URL getEndpointUrl() {
        return mEndpointUrl;
    }

    public void setEndpointUrl(URL endpointUrl) {
        mEndpointUrl = endpointUrl;
    }

    public int getUrlPort() {
        if (mEndpointUrl.getPort() == -1) {
            return mEndpointUrl.getDefaultPort();
        }
        return mEndpointUrl.getPort();
    }

    public String getUrlContext() {
        return mEndpointUrl.getPath();
    }

    public ServiceEndpoint getEndpointReference() {
        return mServiceEndpoint;
    }

    public void setEndpointReference(ServiceEndpoint serviceEndpoint) {
        mServiceEndpoint = serviceEndpoint;
    }

    public String getEndpointName() {
        return mEndpointName;
    }

    public void setEndpointName(String endpointName) {
        mEndpointName = endpointName;
    }

    public QName getServiceName() {
        return mServiceName;
    }

    public void setServiceName(QName serviceName) {
        mServiceName = serviceName;
    }

    public QName getInterfaceName() {
        return mInterfaceName;
    }
    
    public void setInterfaceName(QName interfaceName) {
        mInterfaceName = interfaceName;
    }

    public boolean isInbound() {
        return mIsInbound;
    }

    public void setIsInbound(boolean isInbound) {
        mIsInbound = isInbound;
    }
    
//    public boolean getHostnameVerification() {
//        return mHostnameVerification;
//    }
//
//    
//    public void setHostnameVerification(boolean hostnameVerification) {
//        mHostnameVerification = hostnameVerification;
//    }

    public HttpClientConnectionProperties getHttpClientConnectionProperties() {
        return mHttpClientConnProps ;
    }

    
    public void setHttpClientConnectionProperties(HttpClientConnectionProperties clientConnProps) {
        mHttpClientConnProps  = clientConnProps;
    }
    
    public boolean getPropagateSoapHeader() {
        return mPropagateSoapHeader;
    }
    
    public void setPropagateSoapHeader(boolean propagateSoapHeader) {
        this.mPropagateSoapHeader = propagateSoapHeader;
    }
    
    public Map getOperationNameToMetaData() {
        return mOperationNameToMetaData;
    }

    public void setOperationNameToMetaData(Map opNameToMetaData) {
        mOperationNameToMetaData = opNameToMetaData;
    }


    public BindingOperation[] getOperations(Definition def, String serviceName, String endpointName) {
        Binding binding = getBinding(def, serviceName, endpointName);
        if (binding != null) {
            List l = binding.getBindingOperations();
            if (l != null && l.size() > 0) {
                return (BindingOperation[]) l.toArray(new BindingOperation[0]);
            }
        }
        return null;
    }

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

    public Map getInputMimeParts(BindingInput bi) {
        if (bi == null) {
            throw new IllegalArgumentException(mMessages.getString("HTTPBC-E00357.BindingInput_null"));
        }
        Map mimeMap = new HashMap();
        List mimeParts = new ArrayList();
        List extensions = bi.getExtensibilityElements();
        Iterator extIter = (extensions == null) ? null : extensions.iterator();
        while (extIter != null && extIter.hasNext()) {
            ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
            if (MIMEMultipartRelated.class.isInstance(ee)) {
            	MIMEMultipartRelated mimeMultipartsRelated = (MIMEMultipartRelated) ee;
                mimeParts = mimeMultipartsRelated.getMIMEParts();
                break;
            }
        }
        
        // get the mime content extensibility element
        extIter = mimeParts.iterator();
        while (extIter.hasNext()) {
            MIMEPart mimePart = (MIMEPart) extIter.next();
            extensions = mimePart.getExtensibilityElements();
            // there should be only one extension element, e.g. <mime:content> or <soap:body>
            ExtensibilityElement ee = (ExtensibilityElement) extensions.get(0);
            if (MIMEContent.class.isInstance (ee)) {
                mimeMap.put(mimePart, (MIMEContent) ee);
            }
        }
        
        return mimeMap;
    }
    
    public Map getOutputMimeParts(BindingOutput bo) {
        if (bo == null) {
            throw new IllegalArgumentException(mMessages.getString("HTTPBC-E00358.BindingOutput_null"));
        }
        Map mimeMap = new HashMap();
        List mimeParts = new ArrayList();
        List extensions = bo.getExtensibilityElements();
        Iterator extIter = (extensions == null) ? null : extensions.iterator();
        while (extIter != null && extIter.hasNext()) {
            ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
            if (MIMEMultipartRelated.class.isInstance(ee)) {
            	MIMEMultipartRelated mimeMultipartsRelated = (MIMEMultipartRelated) ee;
                mimeParts = mimeMultipartsRelated.getMIMEParts();
                break;
            }
        }
        
        // get the mime content extensibility element
        extIter = mimeParts.iterator();
        while (extIter.hasNext()) {
            MIMEPart mimePart = (MIMEPart) extIter.next();
            extensions = mimePart.getExtensibilityElements();
            // there should be only one extension element, e.g. <mime:content> or <soap:body>
            ExtensibilityElement ee = (ExtensibilityElement) extensions.get(0);
            if (MIMEContent.class.isInstance (ee)) {
                mimeMap.put(mimePart, (MIMEContent) ee);
            }
        }
        
        return mimeMap;
    }

    /**
     * Determine the message exchange pattern.
     * For handling 1.1 wsdls, map transmission primitives to the
     * closest message exchange pattern, taking into account the endpoint direction
     * direction inbound:
     *      request-response and solicit-response -> in-out
     *      one-way and notification -> in-only
     * direction outbound:
     *      request-response and solicit-response -> out-in
     *      one-way and notification -> out-only
     * @param pm the endpoint configuration from the portmap
     * @param po the binding operation definition from the wsdl
     * @return the message exchange pattern, null if no mapping could be determined.
     */
    String determineMEP(BindingOperation bo) {
        String mep = null;
        OperationType type = bo.getOperation().getStyle();
        if (mIsInbound) {
            if (type.equals(OperationType.REQUEST_RESPONSE) || type.equals(OperationType.SOLICIT_RESPONSE)) {
                mep = "inout"; // NOI18N
            } else if (type.equals(OperationType.ONE_WAY) || type.equals(OperationType.NOTIFICATION)) {
                mep = "inonly"; // NOI18N
            }
        } else {
            if (type.equals(OperationType.REQUEST_RESPONSE) || type.equals(OperationType.SOLICIT_RESPONSE)) {
                mep = "outin"; // NOI18N
            } else if (type.equals(OperationType.ONE_WAY) || type.equals(OperationType.NOTIFICATION)) {
                mep = "outonly"; // NOI18N
            }
        }
        return mep;
    }

        
    public Port getPort() {
        Port port = null;
        Service svc = mDefinition.getService(mServiceName);
        if (svc == null) {
            return null;
        }
        port = svc.getPort(QName.valueOf(mEndpointName).getLocalPart());            
        return port;
    }

    protected PolicyReference getPolicyReference() {
        Port port = getPort();
        if (port != null) {
            List extElems = port.getExtensibilityElements();
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext()) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (PolicyReference.class.isInstance(ee)) {
                    return (PolicyReference)ee;
                }
            }
        }
        return null;   
    }
    
    public boolean isBasicAuthenticationEnabled() {
        PolicyReference reference = getPolicyReference();
        if(reference != null) {
            String policyReferenceURI = reference.getURI();
            if(policyReferenceURI == null) {
                return false;
            } else {
                Policy pol = getPolicy(reference);
                if(pol != null) {
                  return pol.getMustSupportBasicAuthentication().getAuthEnabled().booleanValue();
                }
            }
        }
        return false;
    }

    public Policy getPolicy(PolicyReference pr) {
        Policy p = null;
        if(pr != null) {
            List extElems = mDefinition.getExtensibilityElements();
            Iterator extIter = extElems == null ? null : extElems.iterator();
            while (extIter != null && extIter.hasNext()) {
                ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
                if (Policy.class.isInstance(ee)) {
                    if(((Policy)ee).getID().equalsIgnoreCase(pr.getURI().substring(1)))
                        p = (Policy)ee;
                }
            }
        }
        return p;
    }
    
    public QName createOperationAddress(OperationMetaData opMetaData) {
        String opName = opMetaData.getOperationName();
        if (mInterfaceName == null) {
            Binding binding = getBinding(mDefinition, mServiceName.toString(), mEndpointName);
            mInterfaceName = binding.getPortType().getQName();
        }
        return new QName(mInterfaceName.getNamespaceURI(), opName);
    }
    
    public String getServiceUnitID() {
        return mServiceUnitID;
    }
    
    public String getServiceUnitRootPath() {
        return mServiceUnitRootPath;
    }
    
    public int getMaxConcurrencyLimit() {
        return maxConcurrencyLimit;
    }
    
    public void setMaxConcurrencyLimit(int maxConcurrencyLimit) {
        this.maxConcurrencyLimit = maxConcurrencyLimit;
    }
 
    public RedeliveryConfig getRedeliveryConfiguration() {
        return mRedeliveryConfig;
    }
    
    public void setRedeliveryConfiguration(RedeliveryConfig redeliveryConfig) {
        mRedeliveryConfig = redeliveryConfig;
    }
    
    public void setHandlers(List<HttpSoapHandler> handlers) {
        mHandlers = handlers;
    }

    public List<HttpSoapHandler> getHandlers() {
        return mHandlers;
    }
    
    public void setHandlerLibPaths(List libPaths) {
        mHandlerLibPaths = libPaths;
    }
    
    public List getHandlerLibPaths() {
        return mHandlerLibPaths;
    }
    
    
    public void setEnableWsdlQuery(boolean enabled) {
        this.mEnableWsdlQuery = enabled;
    }
    
    public boolean getEnableWsdlQuery() {
        return this.mEnableWsdlQuery;
    }
    
    public void setValidHostnames(String hostnames) {
    	if (hostnames == null || hostnames.trim().equals("")) {
    	    return;
    	}
    	StringTokenizer st = new StringTokenizer(hostnames, ",");
    	while( st.hasMoreTokens()){
            // hold on, we're not validating the hostname specified here. Maybe we could use a regex, 
            // but can we deal with all possible aliases. Maybe it's better to leave it up to the user...
	    mValidHostnames.put(st.nextToken(), "");  
	}
    }
    
    public Map getValidHostnames() {
        return mValidHostnames;
    }
    
    synchronized public int getPendingExchangeReplies() {
        return this.pendingExchanges;
    }
    
    synchronized public void incrementPendingExchangeReplies() {
        this.pendingExchanges++;
    }
    
    synchronized public void decrementPendingExchangeReplies() {        
        this.pendingExchanges--;
        if (throttlingController != null) {
            throttlingController.resumeSuspendedRequests(this);
        }
    }

    public void setInboundRequestThrottlingController (RequestThrottlingController controller) {
        throttlingController = controller;
    }

    public Subject handleSecurity(String header) throws HttpBcSecurityException {
	Subject subj = null;
	boolean isClassloaderSwitched = false;
	if (mAuthenticateClient) {
	    CredentialValidator cv = null;
	    try {
		isClassloaderSwitched = switchClassLoader();

		AuthInfo authInfo = getAuthInfo();
		cv = mCredValidatorMgr.acquireCredentialValidator(getUniqueName(), mSecPolicy, authInfo);
		if (cv != null) {
		    // You will get here only if the webservice is secured with
		    // HTTP Basic authentication;
		    // So, must have the authorization header
		    if (header == null) {
			throw new HttpBcSecurityException(mMessages.getString("HTTPBC-E01023.NoAuthorizationHeader"));
		    }

		    // Only Basic auth is handled for now.
		    if (!header.startsWith("Basic")) {
			throw new HttpBcSecurityException(mMessages.getString("HTTPBC-E01024.BasicAuthorizationHeaderRequired", header));
		    }

		    String trimHeader = header.trim();
		    String decoded = "";
		    try {
			if (base64 == null) {
			    base64 = new Base64Impl();
			}
			decoded = base64.decode(trimHeader.substring(5).trim());
		    } catch (Exception e) {
			QName qualifiedEndpointName = new QName(getServiceName().getNamespaceURI(), getEndpointName());
			String err = mMessages.getString("HTTPBC-E01035.Failed_to_decode_authorization_header", new Object[] { qualifiedEndpointName.toString(),
				e.getLocalizedMessage() });
			throw new HttpBcSecurityException(err, e);
		    }
		    String username = null;
		    String password = null;
		    String[] credentials = decoded.split(":");
		    if (credentials.length == 2) {
			username = credentials[0];
			password = credentials[1];
		    } else {
			// Need to have both username and password
			throw new HttpBcSecurityException(mMessages.getString("HTTPBC-E01033.Invalid_authorization_header", header));
		    }

		    subj = cv.validateCredential(username, password.toCharArray());
		} else {
		    QName qualifiedEndpointName = new QName(getServiceName().getNamespaceURI(), getEndpointName());
		    String err = mMessages.getString("HTTPBC-E01034.Failed_to_acquire_credential_validator", new Object[] { qualifiedEndpointName.toString() });
		    throw new Exception(err);
		}
	    } catch (Exception e) {
		throw new HttpBcSecurityException(e);
	    } finally {
		if (cv != null) {
		    mCredValidatorMgr.releaseCredentialValidator(cv);
		}
		if (isClassloaderSwitched) {
		    ClassLoader cl = Thread.currentThread().getContextClassLoader();
		    Thread.currentThread().setContextClassLoader(cl.getParent());
		}
	    }
	}
	return subj;
    }

    private boolean switchClassLoader() throws MalformedURLException {
	if (mCredValidatorMgr.isAmPolicy(mSecPolicy)) {
	    ClassLoader cl = Thread.currentThread().getContextClassLoader();
	    
	    ClassLoader amClassloader = getAMClientSdkJarClassloader(cl);
	    if(amClassloader != null){
		Thread.currentThread().setContextClassLoader(amClassloader);
		return true;
	    }
	}
	return false;
    }

    private ClassLoader getAMClientSdkJarClassloader(ClassLoader parent) throws MalformedURLException {
        String s = mCredValidatorMgr.getRuntimeConfiguration().getAMClasspath();
        
        if (s == null || s.equals("")){
            return null;
        }else{
            StringTokenizer tokenizer = new StringTokenizer(s,",");
            URL[] urls = new URL[tokenizer.countTokens()];
            int count=0;
            while(tokenizer.hasMoreTokens()){
        	String token = tokenizer.nextToken();
        	File f = new File( token.trim() );
        	if(f.exists()){
        	    urls[count] = f.toURL();
        	    count++;
        	}else{
        	    return null;
        	}
        	
            }
            
            URLClassLoader urlClassloader = new URLClassLoader(urls,parent);
            return urlClassloader;
        }

	
    }

    private AuthInfo getAuthInfo() {
	AuthInfo i = new AuthInfo();

	if (this instanceof HttpEndpoint) {
	    i.setAction(((HttpEndpoint) this).getHttpBindingVerb());
	}
	i.setResource(this.getEndpointUrl().toString());
	return i;
    }

    public HTTPBasicAuthCredential getBasicAuthCredential() {
	return basicAuthCredentials;
    }
    
    public synchronized long getCRMPMessageId() {
        return (++mMessageIdCounter);
    }
    
    protected void initSecuritySupport () throws Exception {
        //check if security has been enabled.
        securityEnabled = isBasicAuthenticationEnabled();
                
        // if security is enabled and this is "inbound", we need to
        // get the information needed to figure out what kind of authentication 
        // system to use for authenticating the client (i.e., AccessManager, Realm, etc.)
        if (securityEnabled) {
            PolicyReference pr = getPolicyReference();
            if (pr == null) {
                QName qualifiedEndpointName =
                    new QName(getServiceName().getNamespaceURI(), getEndpointName());
                String err = mMessages.getString("HTTPBC-E01031.Invalid_endpoint_config_no_policyref", 
                                        new Object[] {qualifiedEndpointName.toString()});
                throw new Exception (err);                
            } else {
                // the authentication subsystem info is available in the policy extensibility element
                mSecPolicy = getPolicy(pr);
                if (mSecPolicy == null) {
                    QName qualifiedEndpointName =
                        new QName(getServiceName().getNamespaceURI(), getEndpointName());
                    String err = mMessages.getString("HTTPBC-E01032.Invalid_endpoint_config_no_policy", 
                                            new Object[] {qualifiedEndpointName.toString(), getPolicyReference().getURI()});
                    throw new Exception (err);                
                }
            }
            if (isInbound()) {
                mAuthenticateClient = true;
            } else { // outbound, need to register to authenticator to authenticate properly on callback
                if (basicAuthCredentials == null){
                    basicAuthCredentials = new HTTPBasicAuthCredential(mSecPolicy.getUserName(),
                                                                       mSecPolicy.getPassword().toCharArray());
                }
            }

        }
    }
    
    // no-op for now
    public void shutdown() throws JBIException {
    }
}
