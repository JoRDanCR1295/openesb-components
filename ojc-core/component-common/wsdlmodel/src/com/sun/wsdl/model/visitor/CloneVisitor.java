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
 * @(#)CloneVisitor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.visitor;

import com.sun.jbi.internationalization.Messages;
import java.util.logging.Logger;
import java.util.logging.Level;


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
import com.sun.wsdl.model.common.model.EInsightModelException;
import com.sun.wsdl.model.common.model.XMLComment;
import com.sun.wsdl.model.common.model.XMLElement;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.XMLProcessingInstruction;
import com.sun.wsdl.model.common.visitor.CloneSupport;
import com.sun.wsdl.model.common.visitor.ParentChildrenParentVisitor;
import com.sun.wsdl.model.common.visitor.XMLCloneVisitorException;
import com.sun.wsdl.model.common.visitor.CloneSupport.Instantiable;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.extensions.soap.SOAPAddress;
import com.sun.wsdl.model.extensions.soap.SOAPBinding;
import com.sun.wsdl.model.extensions.soap.SOAPBody;
import com.sun.wsdl.model.extensions.soap.SOAPFault;
import com.sun.wsdl.model.extensions.soap.SOAPHeader;
import com.sun.wsdl.model.extensions.soap.SOAPHeaderFault;
import com.sun.wsdl.model.extensions.soap.SOAPOperation;
import com.sun.wsdl.model.impl.WSDLDocumentImpl;




/**
 * Visits the model nodes and clones them.
 *
 * @author Sun Microsystems
 * @version 
 */
public class CloneVisitor extends AbstractVisitor implements ParentChildrenParentVisitor {
    
    /** The logger. */
    private static final Messages MESSAGES = 
            Messages.getMessages(SAXWriteVisitor.class);
    private static final Logger LOGGER = 
            Messages.getLogger(SAXWriteVisitor.class);
    
    /** Creates a new instance of CloneVisitor */
    public CloneVisitor() {
    }
    
    /** Gets the clone visitor support.
     * @return  Visitor support.
     */
    public CloneSupport getCloneSupport() {
        if (null == getVisitorSupport()) {
            setVisitorSupport(new CloneSupport());
        }
        return (CloneSupport) getVisitorSupport();
    }
    
    /** Prepares the visitor for use.
     * @param   v   Values to use.
     *              <ol start="0">
     *              <li><code>com.sun.wsdl.model.common.model.bpel.BPELDocument</code> Destination document.</li>
     *              </ol>
     */
    public void prepare(Object[] v) {
        getCloneSupport().setInstantiatingDocument((WSDLDocument) v[0]);
        getCloneSupport().setResults((XMLNode[]) v[1]);
    }
    
    /** Visits a comment section.
     * @param   c   XML comment section
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(XMLComment c) {
        try {
            XMLComment clone = getCloneSupport().getInstantiatingDocument().createXmlComment();
            clone.setValue(c.getValue());
            if (getCloneSupport().peekCurrentClone() != null) {
                getCloneSupport().peekCurrentClone().addChild(clone);
            }
            getCloneSupport().setCloned(clone);
        } catch (Throwable trw) {
            throw new XMLCloneVisitorException(
                MESSAGES.getString("CloneVisitor.CloneVisitor.VISIT(XMLComment)",new Object[] {}), trw);
        }
        return true;
    }
    
    /** Visits a XML processing instruction section.
     * @param   p   XML processing instruction.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(XMLProcessingInstruction p) {
        try {
            XMLProcessingInstruction clone = getCloneSupport().getInstantiatingDocument()
                .createProcessingInstruction();
            clone.setTarget(p.getTarget());
            clone.setData(p.getData());
            if (getCloneSupport().peekCurrentClone() != null) {
                getCloneSupport().peekCurrentClone().addChild(clone);
            }
            getCloneSupport().setCloned(clone);
        } catch (Throwable trw) {
            throw new XMLCloneVisitorException(
                MESSAGES.getString("CloneVisitor.CloneVisitor.VISIT(XMLProcessingInstruction)",new Object[] {}), trw);
        }
        return true;
    }
    
    /**
     * Visits a WSDL document.
     * @param d the document to visit
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(final WSDLDocument d) {
        LOGGER.log(Level.FINE,
                MESSAGES.getString(
                "CloneVisitor.VISIT(WSDLDocument)"));
        try {
            getCloneSupport().cloneElement(d, new Instantiable() {
                /** @see CloneSupport.Instantiable#create
                 */
                public XMLElement create() {
                    return new WSDLDocumentImpl();
                }

                /** @see CloneSupport.Instantiable#postCloneRun
                 */
                public void postCloneRun(XMLElement clone) {
                }
            });
        } catch (Throwable trw) {
            throw new XMLCloneVisitorException(
                MESSAGES.getString(
                    "CloneVisitor.VISIT(WSDLDocument)"), trw);
        }
        return true;
    }
    
    /**
     * Visits a WSDL definitions.
     * @param w a WSDL definitions element
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(WSDLDefinitions w) {
        LOGGER.log(Level.FINE,
                MESSAGES.getString("CloneVisitor.VISIT(WSDLDefinitions)"));
        getCloneSupport().cloneElement(w, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createDefinitions();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
                // Since we're cloning the definitions element then this is the only to set the
                // target document's root element (if not already set) so that other cloned elements
                // can define namespaces
                if (((WSDLDocument) clone.getOwnerDocument()).getDocumentDefinitions() == null) {
                    ((WSDLDocument) clone.getOwnerDocument()).setDocumentDefinitions((WSDLDefinitions) clone);
                }
            }
        });
        return true;
    }
    
    /** Visits a wsdl:message element.
     * @param   w   a wsdl:message element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(WSDLMessage w) {
        LOGGER.log(Level.FINE,
                MESSAGES.getString("CloneVisitor.VISIT(WSDLMessage)"));
        getCloneSupport().cloneElement(w, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createWSDLMessage();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a part element.
     * @param   p   a part element
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean visit(final Part p) {
        LOGGER.log(Level.FINE,
                MESSAGES.getString("CloneVisitor.VISIT(WSDLPart)"));
        getCloneSupport().cloneEmptyElement(p, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createPart();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
                //((Part) clone).setMetadataRepositoryObject(p.getMetadataRepositoryObject());
            }
        });
        return true;
   }
    
    /**
     * Visits a portType element.
     * @param portType a portType element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(PortType portType) {
        LOGGER.log(Level.FINE,
                MESSAGES.getString("CloneVisitor.VISIT(WSDLPortType)"));
        getCloneSupport().cloneElement(portType, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createPortType();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * Visits a portType operation element.
     * @param operation a portType operation element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Operation operation) {
        getCloneSupport().cloneElement(operation, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createOperation();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * Visits an operation input element.
     * @param input an operation input element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(OperationInput input) {
        getCloneSupport().cloneEmptyElement(input, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createOperationInput();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * Visits an operation output element.
     * @param output an operation output element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(OperationOutput output) {
        getCloneSupport().cloneEmptyElement(output, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createOperationOutput();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * Visits an operation fault element.
     * @param fault an operation fault element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(OperationFault fault) {
        getCloneSupport().cloneEmptyElement(fault, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createOperationFault();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * Visits a service element.
     * @param service a service element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Service service) {
        getCloneSupport().cloneElement(service, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createService();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * Visits a service port element.
     * @param port a service port element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(ServicePort port) {
        getCloneSupport().cloneElement(port, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createServicePort();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * Visits a service link type element.
     * @param type a service link type element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(PartnerLinkType type) {
        getCloneSupport().cloneElement(type, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createPartnerLinkType();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * Visits a service link role element.
     * @param role a service link role element
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(PartnerLinkRole role) {
        getCloneSupport().cloneElement(role, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createPartnerLinkRole();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * Visits an import element.
     * @param wsdlImport an import element
     * @return <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean visit(Import wsdlImport) {
        getCloneSupport().cloneEmptyElement(wsdlImport, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createImport();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(Binding)
     */
    public boolean visit(Binding binding) {
        getCloneSupport().cloneElement(binding, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createBinding();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPBinding)
     */
    public boolean visit(SOAPBinding sBinding) {
        getCloneSupport().cloneEmptyElement(sBinding, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createSOAPBinding();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingOperation)
     */
    public boolean visit(BindingOperation bindingOp) {
        getCloneSupport().cloneElement(bindingOp, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createBindingOperation();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPOperation)
     */
    public boolean visit(SOAPOperation sOperation) {
        getCloneSupport().cloneEmptyElement(sOperation, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createSOAPOperation();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingInput)
     */
    public boolean visit(BindingInput bindingIn) {
        getCloneSupport().cloneElement(bindingIn, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createBindingInput();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingOutput)
     */
    public boolean visit(BindingOutput bindingOut) {
        getCloneSupport().cloneElement(bindingOut, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createBindingOutput();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(BindingFault)
     */
    public boolean visit(BindingFault bindingFault) {
        getCloneSupport().cloneElement(bindingFault, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createBindingFault();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPBody)
     */
    public boolean visit(SOAPBody sBody) {
        getCloneSupport().cloneEmptyElement(sBody, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createSOAPBody();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPHeader)
     */
    public boolean visit(SOAPHeader sHeader) {
        getCloneSupport().cloneElement(sHeader, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createSOAPHeader();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPHeaderFault)
     */
    public boolean visit(SOAPHeaderFault sHeaderFault) {
        getCloneSupport().cloneEmptyElement(sHeaderFault, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createSOAPHeaderFault();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPAddress)
     */
    public boolean visit(SOAPAddress sAddress) {
        getCloneSupport().cloneEmptyElement(sAddress, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createSOAPAddress();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(SOAPFault)
     */
    public boolean visit(SOAPFault sFault) {
        getCloneSupport().cloneEmptyElement(sFault, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createSOAPFault();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * @see DocumentationVisitor#visit(Documentation)
     */
    public boolean visit(Documentation doc) {
        LOGGER.log(Level.FINE,
                MESSAGES.getString("CloneVisitor.VISIT(Documentation)"));
        try {
            if (doc.getValue() != null) {
                Documentation clone = ((WSDLDocument) getCloneSupport().getInstantiatingDocument())
                    .createWSDLDocumentation();
                clone.setValue(doc.getValue());
                if (getCloneSupport().peekCurrentClone() != null) {
                    getCloneSupport().peekCurrentClone().addChild(clone);
                }
                getCloneSupport().setCloned(clone);
            }
        } catch (Throwable trw) {
            throw new XMLCloneVisitorException(
                MESSAGES.getString("CloneVisitor.CloneVisitor.VISIT(Documentation)",new Object[] {}), trw);
        }
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(Types)
     */
    public boolean visit(Types types) {
        getCloneSupport().cloneElement(types, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createTypes();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /**
     * @see WSDLVisitor#visit(ExtensibilityElement)
     */
    public boolean visit(ExtensibilityElement ext) {
        getCloneSupport().cloneElement(ext, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createExtensibilityElement();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a property element.
     * @param   p   a property element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Property p) {
        getCloneSupport().cloneEmptyElement(p, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createBPWSProperty();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a propertyAlias element.
     * @param   p   a propertyAlias element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(PropertyAlias p) {
        getCloneSupport().cloneEmptyElement(p, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createBPWSPropertyAlias();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    
    /** Visits a Query element.
     * @param   q   a Query element.
     * @return  <code>true</code> if auto-traversal is to continue.
     */
    public boolean visit(Query q) {
        getCloneSupport().cloneEmptyElement(q, new Instantiable() {
            /** @see CloneSupport.Instantiable#create
             */
            public XMLElement create() {
                return ((WSDLDocument) getCloneSupport().getInstantiatingDocument()).createBPWSQuery();
            }

            /** @see CloneSupport.Instantiable#postCloneRun
             */
            public void postCloneRun(XMLElement clone) {
            }
        });
        return true;
    }
    

}
