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
 * @(#)WSDLModel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.http.HTTPBinding;
import javax.wsdl.extensions.mime.MIMEContent;
import javax.wsdl.extensions.mime.MIMEMultipartRelated;
import javax.wsdl.extensions.soap.SOAPBinding;
import javax.wsdl.extensions.soap.SOAPBody;
import javax.wsdl.extensions.soap.SOAPHeader;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.XmlException;
import org.w3c.dom.Element;

import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.parsers.WSDLParser;

/**
 * @author Graj
 */
public interface WSDLModel {

    /**
     * @return Returns the wsdlParser.
     */
    public WSDLParser getWsdlParser();

    /**
     * @return Returns the xsdHelper.
     */
    public XSDModel getXsdHelper();

    /**
     * @return Returns the WSDL definition.
     */
    public Definition getDefinition();

    /**
     * 
     * @param wsdlURI
     *            a URI (can be a filename or URL) pointing to a WSDL XML
     *            definition.
     */
    public void populate(String wsdlURI) throws WSDLException, XmlException;

    /**
     * @return
     */
    public QName getDefinitionQName();

    /**
     * @return
     */
    public Map getNamespaces();

    /**
     * @return
     */
    public Map getImports();

    /**
     * @return
     */
    public String getTargetNamespace();

    /**
     * 
     */
    public String getWSDLString(String wsdlURI);

    /**
     * Retrieves an ArrayList of ServiceImpl Objects
     * 
     * @return an ArrayList of ServiceImpl Objects
     */
    public Service[] retrieveServices();

    // //////////////////////////////////////////////
    // -- SERVICE PORT HELPERS --
    // //////////////////////////////////////////////
    /**
     * Given a service object, retrieves an ArrayList of PortImpl Objects
     * 
     * @return an ArrayList of PortImpl Objects
     */
    public Port[] retrieveServicePorts(Service service);

    // //////////////////////////////////////////////
    // -- BINDING HELPERS --
    // //////////////////////////////////////////////
    /**
     * @return true if SOAPBinding port, false if not
     */
    public boolean hasSOAPBindingPort(Port port);

    /**
     * @return true if SOAPBinding, false if not
     */
    public boolean isSOAPBinding(Binding binding);

    /**
     * Given a binding object return elements of SOAP Binding
     * 
     * @param binding
     * @return
     */
    public SOAPBinding[] getSOAPBindings(Binding binding);

    /**
     * Given a binding object return elements of SOAP Binding
     * 
     * @param binding
     * @return
     */
    public HTTPBinding[] getHTTPBindings(Binding binding);

    /**
     * Given a binding object and an operation object, return bindingOperation
     * 
     * @param binding
     * @param operation
     * @return
     */
    public BindingOperation findBindingOperation(Binding binding,
            Operation operation);

    /**
     * Given a binding input object return it's SOAPHeader header, message,
     * part, use, etc.
     * 
     * @param input
     * @return
     */
    public SOAPHeader getSOAPBindingInputHeader(BindingInput input);

    /**
     * Given a binding input object return it's SOAPBody header, message, part,
     * use, etc.
     * 
     * @param input
     * @return
     */
    public SOAPBody getSOAPBindingInputBody(BindingInput input);

    /**
     * 
     * @param input
     * @param className
     * @return
     */
    public Object getBindingInputElement(BindingInput input, String className);

    /**
     * Given a binding output object return it's SOAPHeader header, message,
     * part, use, etc.
     * 
     * @param output
     * @return a SOAPHeader object or null
     */
    public SOAPHeader getSOAPBindingOutputHeader(BindingOutput output);

    /**
     * Given a binding output object return it's SOAPBody header, message, part,
     * use, etc.
     * 
     * @param output
     * @return a SOAPBody object or null
     */
    public SOAPBody getSOAPBindingOutputBody(BindingOutput output);

    /**
     * 
     * @param output
     * @param className
     * @return
     */
    public Object getBindingOutputElement(BindingOutput output, String className);

    // //////////////////////////////////////////////
    // -- SERVICE HELPERS --
    // //////////////////////////////////////////////
    public String getDocumentation(Element element);

    public Service addNewSOAPServiceForBinding(Definition definition, QName newServiceName,
            String newPortName, String newLocationUri, QName bindingName)
            throws WSDLException;

    // //////////////////////////////////////////////
    // -- PART HELPERS --
    // //////////////////////////////////////////////
    public Map getParameters();

    
    // //////////////////////////////////////////////
    // -- OTHER HELPERS --
    // //////////////////////////////////////////////
    /**
     * Retrieves all the SOAPBinding elements in the WSDL
     */
    public String[] getAllSOAPLocationURIs();

    /**
     * Retrieves all the HTTPBinding elements in the WSDL
     */
    public String[] getAllHTTPLocationURIs();

    public PortType getSOAPAbstractPortType(QName serviceName, String portName);

    public PortType getSOAPAbstractPortType(String serviceName, String portName);

    public Part[] getInputParts(Operation operation);

    public Part[] getOutputParts(Operation operation);

    public String getSOAPLocationURI(Port port);
    
    public String setSOAPLocationURI(Port port, String newLocationURI);

    public ExtensibilityElement getExtensiblityElement(List list, Class clazz );

    public ExtensibilityElement [] getExtensiblityElements(List list, Class clazz );

    public String getSoapAction(BindingOperation operation);

    public String[] getEndpointsForBinding(Definition definition, Binding binding);

    public Binding findBindingForOperation(Definition definition, BindingOperation bindingOperation);

    public boolean isInputSoapEncoded(BindingOperation bindingOperation);

    public boolean isOutputSoapEncoded(BindingOperation bindingOperation);

    public boolean isRpc(Definition definition, BindingOperation bindingOperation);

    public boolean isRpc(Binding binding);

    public Part[] getInputParts(BindingOperation operation);

    public Part[] getOutputParts(BindingOperation operation);

    public Part[] getFaultParts(BindingOperation bindingOperation, String faultName);

    public boolean isAttachmentInputPart(Part part, BindingOperation operation);

    public boolean isAttachmentOutputPart(Part part, BindingOperation operation);

    public MIMEContent[] getInputMultipartContent(Part part, BindingOperation operation);

    public MIMEContent[] getOutputMultipartContent(Part part, BindingOperation operation);

    public MIMEContent[] getContentParts(Part part, MIMEMultipartRelated multipart);

    public boolean isMultipartRequest(Definition definition, BindingOperation bindingOperation);
    
    public void writeWSDLToFile(Definition definition, String wsdlFileName) throws WSDLException,
    IOException;
}
