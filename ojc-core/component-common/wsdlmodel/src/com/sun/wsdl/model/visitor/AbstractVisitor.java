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
 * @(#)AbstractVisitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.visitor;

import com.sun.wsdl.model.Binding;
import com.sun.wsdl.model.BindingFault;
import com.sun.wsdl.model.BindingInput;
import com.sun.wsdl.model.BindingOperation;
import com.sun.wsdl.model.BindingOutput;
import com.sun.wsdl.model.Import;
import com.sun.wsdl.model.Operation;
import com.sun.wsdl.model.OperationFault;
import com.sun.wsdl.model.OperationInput;
import com.sun.wsdl.model.OperationOutput;
import com.sun.wsdl.model.Part;
import com.sun.wsdl.model.PortType;
import com.sun.wsdl.model.Service;
import com.sun.wsdl.model.PartnerLinkRole;
import com.sun.wsdl.model.PartnerLinkType;
import com.sun.wsdl.model.ServicePort;
import com.sun.wsdl.model.Types;
import com.sun.wsdl.model.WSDLDefinitions;
import com.sun.wsdl.model.WSDLDocument;
import com.sun.wsdl.model.WSDLMessage;
import com.sun.wsdl.model.bpel.Property;
import com.sun.wsdl.model.bpel.PropertyAlias;
import com.sun.wsdl.model.bpel.Query;
import com.sun.wsdl.model.common.model.Documentation;
import com.sun.wsdl.model.common.model.XMLComment;
import com.sun.wsdl.model.common.model.XMLProcessingInstruction;
import com.sun.wsdl.model.common.model.XMLText;
import com.sun.wsdl.model.common.visitor.CommentVisitor;
import com.sun.wsdl.model.common.visitor.DocumentationVisitor;
import com.sun.wsdl.model.common.visitor.ProcessingInstructionVisitor;
import com.sun.wsdl.model.common.visitor.TextVisitor;
import com.sun.wsdl.model.common.visitor.VisitorImpl;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.extensions.soap.SOAPAddress;
import com.sun.wsdl.model.extensions.soap.SOAPBinding;
import com.sun.wsdl.model.extensions.soap.SOAPBody;
import com.sun.wsdl.model.extensions.soap.SOAPFault;
import com.sun.wsdl.model.extensions.soap.SOAPHeader;
import com.sun.wsdl.model.extensions.soap.SOAPHeaderFault;
import com.sun.wsdl.model.extensions.soap.SOAPOperation;

/**
 * A basic implementation of a visitor to a WSDL document.
 *
 * @author Sun Microsystems
 * @version 
 */
public abstract class AbstractVisitor extends VisitorImpl
    implements WSDLVisitor,
               CommentVisitor,
               ProcessingInstructionVisitor,
               TextVisitor,
               DocumentationVisitor {
    
    /** Creates a new instance of AbstractVisitor */
    public AbstractVisitor() {
        super();
    }
    
    /**
     * @see WSDLVisitor#visit(XMLComment)
     */
    public boolean visit(XMLComment c) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(XMLProcessingInstruction)
     */
    public boolean visit(XMLProcessingInstruction p) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(XMLText)
     */
    public boolean visit(XMLText t) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(WSDLDocument)
     */
    public boolean visit(WSDLDocument d) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(WSDLDefinitions)
     */
    public boolean visit(WSDLDefinitions d) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(WSDLMessage)
     */
    public boolean visit(WSDLMessage w) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(Part)
     */
    public boolean visit(Part p) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(PortType)
     */
    public boolean visit(PortType portType) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(Operation)
     */
    public boolean visit(Operation operation) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(OperationInput)
     */
    public boolean visit(OperationInput input) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(OperationOutput)
     */
    public boolean visit(OperationOutput output) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(OperationFault)
     */
    public boolean visit(OperationFault fault) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(Service)
     */
    public boolean visit(Service service) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(ServicePort)
     */
    public boolean visit(ServicePort port) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(PartnerLinkType)
     */
    public boolean visit(PartnerLinkType type) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(PartnerLinkRole)
     */
    public boolean visit(PartnerLinkRole role) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(Import)
     */
    public boolean visit(Import wsdlImport) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(Binding)
     */
    public boolean visit(Binding binding) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPBinding)
     */
    public boolean visit(SOAPBinding sBinding) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingOperation)
     */
    public boolean visit(BindingOperation bindingOp) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPOperation)
     */
    public boolean visit(SOAPOperation sOperation) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingInput)
     */
    public boolean visit(BindingInput bindingIn) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingOutput)
     */
    public boolean visit(BindingOutput bindingOut) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingFault)
     */
    public boolean visit(BindingFault bindingFault) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPBody)
     */
    public boolean visit(SOAPBody sBody) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPHeader)
     */
    public boolean visit(SOAPHeader sHeader) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPHeaderFault)
     */
    public boolean visit(SOAPHeaderFault sHeaderFault) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPAddress)
     */
    public boolean visit(SOAPAddress sAddress) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPFault)
     */
    public boolean visit(SOAPFault sFault) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(Documentation)
     */
    public boolean visit(Documentation doc) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(Types)
     */
    public boolean visit(Types types) {
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(ExtensibilityElement)
     */
    public boolean visit(ExtensibilityElement ext) {
        return true;
    }
    
    /** Visits a property element.
     * @param   p   a property element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Property p) {
        return true;
    }
    
    /** Visits a propertyAlias element.
     * @param   p   a propertyAlias element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(PropertyAlias p) {
        return true;
    }
    
    /** Visits a Query element.
     * @param   q   a Query element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Query q) {
    	return true;
    }
}
