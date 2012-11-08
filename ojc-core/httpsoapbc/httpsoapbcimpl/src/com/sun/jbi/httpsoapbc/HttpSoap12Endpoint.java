/**
 * 
 */
package com.sun.jbi.httpsoapbc;

import java.net.InetAddress;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.extensions.mime.MIMEContent;
import javax.wsdl.extensions.mime.MIMEPart;
import javax.wsdl.extensions.mime.MIMEMultipartRelated;
import javax.wsdl.Output;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.soap12.SOAP12Address;
import javax.wsdl.extensions.soap12.SOAP12Binding;
import javax.wsdl.extensions.soap12.SOAP12Body;
import javax.wsdl.extensions.soap12.SOAP12Header;
import javax.wsdl.extensions.soap12.SOAP12Operation;
import javax.xml.namespace.QName;
import javax.xml.ws.Service;

import com.sun.jbi.httpsoapbc.security.api.EndpointSecurityConfig;
import com.sun.jbi.httpsoapbc.security.impl.CredentialValidatorManager;

/**
 * @author Sujit Biswas
 *
 */
public class HttpSoap12Endpoint extends HttpSoapEndpoint {

    private static final long serialVersionUID = 1L;
    
    private static  Logger mLogger = mMessages.getLogger(HttpSoap12Endpoint.class);

    /**
     * @param def
     * @param serviceName
     * @param endpointName
     * @param interfaceName
     * @param isProvider
     * @param wsdlPath
     * @param securityConfig
     * @param serviceUnitID
     * @param serviceUnitRootPath
     * @param cvm
     */
    public HttpSoap12Endpoint(
	    Definition def, 
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
	super(def,serviceName,endpointName,interfaceName,isProvider,clientConnProps, propagateSoapHeader, wsdlPath,securityConfig,serviceUnitID,serviceUnitRootPath,cvm);
	
    }

    public void init() throws Exception {

	// If it has a soap binding, we need a soap address.  soap:address
	// should be a valid HTTP URL when we reach here. 
	String location = getEndpointUrl(getPort());

	// if HTTP BC is providing the service, i.e. a consumer of the endpoint, 
	// only "localhost" is allowed as the host in the URL location
	if (mIsInbound) {
	    String hostname = new URL(location).getHost();
	    if (! ( InetAddress.getLocalHost().getCanonicalHostName().equalsIgnoreCase(hostname) || "localhost".equalsIgnoreCase(hostname)) && 
	        !mValidHostnames.containsKey(hostname) ) {
		throw new Exception(mMessages.getString("HTTPBC-E00298.Invalid_host_name", new Object[] { hostname, location, mWsdlPath }));
	    }
	}

	////////////////////////////////////////////////////////
	// GSR changed for Java EE Service Engine
	// As instructed by Jerry Waldorf.
	////////////////////////////////////////////////////////
	if ("REPLACE_WITH_ACTUAL_URL".equals(location)) {
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
	    String exMsg = mMessages.getString("HTTPBC-E00353.Endpoint_no_operation", new Object[] { getEndpointName() });
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
		    String exMsg = mMessages.getString("HTTPBC-E00354.Operation_no_message", new Object[] { inMsg.getName(), bo.getOperation().getName(), getEndpointName() });
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
		    String exMsg = mMessages.getString("HTTPBC-E00354.Operation_no_message", new Object[] { outMsg.getName(), bo.getOperation().getName(), getEndpointName() });
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
		meta.setInputSoap12Body(getSoap12Body(inputBinding));
		meta.setInputSOAPMimeContents(getInputMimeParts(inputBinding));
	    }
	    BindingOutput outputBinding = bo.getBindingOutput();
	    meta.setBindingOutput(outputBinding);
	    if (outputBinding != null) {
		meta.setOutputSoap12Body(getSoap12Body(outputBinding));
                meta.setOutputSOAPMimeContents(getOutputMimeParts(outputBinding));
	    }
	    
	    meta.setSoap12(true);

	    // soap headers for in/out
	    if (inputBinding != null) {
		meta.setInputSoapHeaders(getSoapHeaders(inputBinding));
	    }
	    if (outputBinding != null) {
		meta.setOutputSoapHeaders(getSoapHeaders(outputBinding));
	    }

	    //Check soap:operation first then soap:binding
	    boolean isDocument = true;
	    String style = getSoapOpStyle(bo);
	    if (style != null) {
		isDocument = style.equalsIgnoreCase("rpc") ? false : true;
	    } else {
		Binding binding = null;
		binding = getBinding(getServiceDescriptor(), getServiceName().toString(), getEndpointName());
		if (binding == null) {
		    String exMsg = mMessages.getString("HTTPBC-E00355.Endpoint_no_binding", new Object[] { getEndpointName() });
		    throw new Exception(exMsg);
		}
		isDocument = isDocumentMode(binding);
	    }
	    if (mLogger.isLoggable(Level.FINE)) {
		mLogger.log(Level.FINE, "isDocumentMode " + isDocument + " for endpoint " + getEndpointName() + " operation " + operationName);
	    }
	    meta.setIsDocumentMode(isDocument);

	    // find out the faults this operation may throw
	    Map m = bo.getOperation().getFaults();
	    if (m != null) {
		meta.setFaults(m);
	    }

	    meta.setFullDefinition(currentDef);

	    if (bo.getOperation().getStyle() == null) {
		String exMsg = mMessages.getString("HTTPBC-E00356.Operation_no_mep", new Object[] { operationName });
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
		    mLogger.log(Level.FINE, "soap actions for operation " + currOperation + " are not unique (" + currSoapAction + "), disabling use of soap action as hints");
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
            if (SOAP12Operation.class.isInstance(ee)) {
        	SOAP12Operation oper = (SOAP12Operation) ee;
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
            if (SOAP12Operation.class.isInstance(ee)) {
        	SOAP12Operation oper = (SOAP12Operation) ee;
                soapOpStyle = oper.getStyle();
            }
                
        }
        return soapOpStyle;
    }
        
    public SOAP12Body getSoap12Body(BindingInput bi) {
        // assert bi != null
        if (bi == null) {
            throw new IllegalArgumentException(mMessages.getString("HTTPBC-E00357.BindingInput_null"));
        }
        return getSoap12Body0(bi.getExtensibilityElements());
    }
        
    protected SOAP12Body getSoap12Body0(List extElems) {
        //soap:body
        Iterator extIter = extElems == null ? null : extElems.iterator();
        while (extIter != null && extIter.hasNext()) {
            ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
            if (SOAP12Body.class.isInstance(ee)) {
        	SOAP12Body body = (SOAP12Body) ee;  // no mime extensions
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
                            if (SOAP12Body.class.isInstance (ee)) {
                                SOAP12Body body = (SOAP12Body) ee;
                                return body;
                            }
                        }
                    }
                }
            }
                
        }
        return null;
    }
        
    public SOAP12Body getSoap12Body(BindingOutput bo) {
        // assert bi != null
        if (bo == null) {
            throw new IllegalArgumentException(mMessages.getString("HTTPBC-E00358.BindingOutput_null"));
        }
        return getSoap12Body0(bo.getExtensibilityElements());
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
            if (SOAP12Header.class.isInstance(ee)) {
        	SOAP12Header header = (SOAP12Header) ee;
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
                            if (SOAP12Header.class.isInstance (ee)) {
                                SOAP12Header header = (SOAP12Header) ee;
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
		if (SOAP12Address.class.isInstance(ee)) {
		    SOAP12Address addr = (SOAP12Address) ee;
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
	    if (SOAP12Binding.class.isInstance(ee)) {
		SOAP12Binding soapBinding = (SOAP12Binding) ee;
		return (soapBinding.getStyle() == null) ? true : "document".equalsIgnoreCase(soapBinding.getStyle()); // NOI18N
	    }

	}
	return true;
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
	    SOAP12Body bodyBinding = meta.getInputSoap12Body();
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
		    String err = mMessages.getString("HTTPBC-E00352.Part_not_in_message", new Object[] { bodyPart, msg.getQName().toString() });
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

}
