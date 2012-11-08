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
 * @(#)WSDLVisitor.java 
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
import com.sun.wsdl.model.common.visitor.AutonomousVisitor;
import com.sun.wsdl.model.common.visitor.ChildrenParentVisitor;
import com.sun.wsdl.model.common.visitor.ParentChildrenParentVisitor;
import com.sun.wsdl.model.common.visitor.ParentChildrenVisitor;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.extensions.soap.SOAPAddress;
import com.sun.wsdl.model.extensions.soap.SOAPBinding;
import com.sun.wsdl.model.extensions.soap.SOAPBody;
import com.sun.wsdl.model.extensions.soap.SOAPFault;
import com.sun.wsdl.model.extensions.soap.SOAPHeader;
import com.sun.wsdl.model.extensions.soap.SOAPHeaderFault;
import com.sun.wsdl.model.extensions.soap.SOAPOperation;

/** Describes a visitor to the WSDL model tree.
 * <B>Do not just implement the interface only, but also one of the See Also
 * interfaces below.</B>
 *
 * @see AutonomousVisitor
 * @see ParentChildrenVisitor
 * @see ChildrenParentVisitor
 * @see ParentChildrenParentVisitor
 *
 * @author Sun Microsystems
 * @version 
 */
public interface WSDLVisitor extends Visitor {
    
    /** Prepares the visitor for use.
     * @param   v   Values to use.
     */
    void prepare(Object[] v);
    
    /** Visits a WSDL document.
     * @param   w   A WSDL document.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(WSDLDocument w);
    
    /**
     * Visits a WSDL definitions.
     * @param w a WSDL definitions element
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(WSDLDefinitions w);
    
    /** Visits a wsdl:message element.
     * @param   w   a wsdl:message element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(WSDLMessage w);
    
    /** Visits a part element.
     * @param   p   a part element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Part p);
    
    /**
     * Visits a portType element.
     * @param portType a portType element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(PortType portType);
    
    /**
     * Visits a portType operation element.
     * @param operation a portType operation element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Operation operation);
    
    /**
     * Visits an operation input element.
     * @param input an operation input element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(OperationInput input);
    
    /**
     * Visits an operation output element.
     * @param output an operation output element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(OperationOutput output);
    
    /**
     * Visits an operation fault element.
     * @param fault an operation fault element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(OperationFault fault);
    
    /**
     * Visits a service element.
     * @param service a service element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Service service);
    
    /**
     * Visits a service port element.
     * @param port a service port element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(ServicePort port);
    
    /**
     * Visits a service link type element.
     * @param type a service link type element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(PartnerLinkType type);
    
    /**
     * Visits a service link role element.
     * @param role a service link role element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(PartnerLinkRole role);
    
    /**
     * Visits an import element.
     * @param wsdlImport an import element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Import wsdlImport);
    
    /**
     * Visits a binding element.
     * @param binding a binding element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Binding binding);
    
    /**
     * Visits a SOAP binding element.
     * @param sBinding a SOAP binding element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(SOAPBinding sBinding);
    
    /**
     * Visits a binding operation element.
     * @param bindingOp A binding operation element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(BindingOperation bindingOp);
    
    /**
     * Visits a SOAP binding operation element.
     * @param sOperation a SOAP binding operation element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(SOAPOperation sOperation);
    
    /**
     * Visits a binding operation input element.
     * @param bindingIn A binding operation input element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(BindingInput bindingIn);
    
    /**
     * Visits a binding operation output element.
     * @param bindingOut A binding operation output element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(BindingOutput bindingOut);
    
    /**
     * Visits a binding operation fault element.
     * @param bindingFault A binding operation fault element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(BindingFault bindingFault);
    
    /**
     * Visits a SOAP binding body element.
     * @param sBody a SOAP binding body element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(SOAPBody sBody);
    
    /**
     * Visits a SOAP header element.
     * @param sHeader a SOAP header element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(SOAPHeader sHeader);
    
    /**
     * Visits a SOAP header fault element.
     * @param sHeaderFault a SOAP header fault element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(SOAPHeaderFault sHeaderFault);
    
    /**
     * Visits a SOAP address element.
     * @param sAddress a SOAP address element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(SOAPAddress sAddress);
    
    /**
     * Visits a SOAP fault element.
     * @param sFault a SOAP fault element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(SOAPFault sFault);
    
    /**
     * Visits a types element.
     * @param types a types element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(Types types);
    
    /**
     * Visits an extensibility element.
     * @param ext an extensibility element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    boolean visit(ExtensibilityElement ext);
    
    /** Visits a property element.
     * @param   p   a property element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Property p);
    
    /** Visits a propertyAlias element.
     * @param   p   a propertyAlias element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(PropertyAlias p);
    
    /** Visits a Query element.
     * @param   q   a Query element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    boolean visit(Query q);
    
}
