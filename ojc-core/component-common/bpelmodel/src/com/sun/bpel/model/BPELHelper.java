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
 * @(#)BPELHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaType;

import com.sun.bpel.model.impl.NamespaceUtility;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.wsdl4j.ext.WSDL4JExt;
import com.sun.wsdl4j.ext.bpel.PartnerLinkType;

/**
 * 
 * @author Sun Microsystems
 *
 */
public class BPELHelper {
	
	/**
	 * Get the first ancestor Scope or BPELProcess which has partnerLink declaration
	 * with given partnerLinkName and return it. 
	 * @param partnerLinkName name of partnerLink
	 * @param element BPELElement from where we start this look up
	 * @return PartnerLink or null if not found
	 */
	public static PartnerLink getMatchingPartnerLink(String partnerLinkName, BPELElement element) {
		PartnerLink partnerLink = null;
		
		if(partnerLinkName != null && element != null) {
			
			XMLNode parent = element.getParent();
			while(parent != null && partnerLink == null) {
				if(parent instanceof Scope) {
					Scope scope = (Scope) parent;
					partnerLink = scope.getBPELPartnerLink(partnerLinkName);
				} else if (parent instanceof BPELProcess) {
					BPELProcess process = (BPELProcess) parent;
					partnerLink = process.getBPELPartnerLink(partnerLinkName);
				}
				
				parent = parent.getParent();
			}
		}
		
		return partnerLink;
	}
	
	/**
	 * Get the first ancestor Scope or BPELProcess which has variable declaration
	 * with given variableName and return it.
	 * @param variableName name of the variable
	 * @param element BPELElement from where we start this look up
	 * @return Variable or null if not found
	 */
	public static Variable getMatchingVariable(String variableName, XMLNode element) {
		Variable variable = null;
		
		if(variableName != null && element != null) {
			
			while(element != null && variable == null) {
				if(element instanceof Scope) {
					Scope scope = (Scope) element;
					variable = scope.getBPELVariable(variableName);
				} else if (element instanceof BPELProcess) {
					BPELProcess process = (BPELProcess) element;
					variable = process.getBPELVariable(variableName);
				} else if (element instanceof VariableScope) {
                    VariableScope scopeActivity = (VariableScope) element;
                    variable = scopeActivity.getBPELVariable(variableName);
                }				
				element = element.getParent();
			}
		}
		
		return variable;
	}

    /**
     * Get CorrelationSet declaration from the specific Scope
     * with given CorrelationSet Name and return it. This is useful in EventHandlers where
     * OnEvent's correlation is resolved to its enclosed Scope first
     * @param cSetName name of the correlation Set
     * @param scope Scope from where we do this look up
     * @return CorrelationSet or null if not found
     */
    public static CorrelationSet getMatchingCorrelationSetToScope(String cSetName, Scope scope) {
        CorrelationSet cSet = null;

        if (cSetName != null && scope != null) {
            cSet = scope.getCorrelationSet(cSetName);
        }

        return cSet;
    }
    
    
    /**
     * Get the first ancestor Scope or BPELProcess which has CorrelationSet declaration with given
     * CorrelationSet Name and return it.
     * 
     * @param cSetName
     *            name of the correlation Set
     * @param element
     *            BPELElement from where we start this look up
     * @return CorrelationSet or null if not found
     */
    public static CorrelationSet getMatchingCorrelationSet(String cSetName, 
            BPELElement element) {
        CorrelationSet cSet = null;
        
        if(cSetName != null && element != null) {
            
            XMLNode parent = element.getParent();
            while(parent != null && cSet == null) {
                if(parent instanceof BPELProcessOrScope) {
                    BPELProcessOrScope scope = (BPELProcessOrScope) parent;
                    cSet = scope.getCorrelationSet(cSetName);
                }                
                parent = parent.getParent();
            }
        }
        
        return cSet;
    }    

    
	/**
	 * Get All the variables which are available for given BPELElement.
	 * local variable will take precedence over global variable.
	 * Meaning if BPELElement is contained in a scope which has
	 * a variable defined with the same name as the variable in process
	 * or any ancestor scope then only local variable available with
	 * that name in first ancestor scope will be returned.
	 * @param variableName name of the variable
	 * @param element BPELElement from where we start this look up
	 * @return Collection of all in scope variables or empty collection
	 */
	public static Collection getAllUniqueVariables(BPELElement element) {
		ArrayList allUniqueVariables  = new ArrayList();
		ArrayList allUniqueVariableNames  = new ArrayList();
		
		Variables variables = null;
		XMLNode parent = element.getParent();
		while(parent != null) {
			if(parent instanceof Scope) {
				Scope scope = (Scope) parent;
				variables = scope.getVariables();
			} else if (parent instanceof BPELProcess) {
				BPELProcess process = (BPELProcess) parent;
				variables = process.getVariables();
			}
			
			if(variables != null) {
				Iterator it = variables.getVariables().iterator();
				while(it.hasNext())  {
					Variable variable = (Variable) it.next();
					if(!allUniqueVariableNames.contains(variable.getName())) {
						allUniqueVariableNames.add(variable.getName());
						allUniqueVariables.add(variable);
					}
				}
			}
			parent = parent.getParent();
		}
	
		
		return allUniqueVariables;
	}
	
	/**
	 * Get the first ancestor scope or bpel process which contains this elemement.
	 * @param element BPELElement whose first ancestor scope or process is returned.
	 * @return BPELProcessOrScope or null if no ancestor of type BPELProcessOrScope is found.
	 */
	public static FaultHandlerScope getFirstAncestorBPELProcessOrScope(BPELElement element) {
		if(element == null) {
			return null;
		}
		
		FaultHandlerScope processOrScope = null;
		
		XMLNode parent = element.getParent();
		while(parent != null) {
			if(parent instanceof BPELProcessOrScope) {
				processOrScope = (FaultHandlerScope) parent;
				break;
			}
			
			parent = parent.getParent();
		}
	
		
		return processOrScope;
	}
	
	public static Link getFlowLink(String linkName, BPELElement element) {
		Link link = null;
		
		if(linkName != null && element != null) {
			XMLNode parent = element.getParent();
			while(parent != null && link == null) {
				if(parent instanceof Flow) {
					Flow flow = (Flow) parent;
					link = flow.getLink(linkName);
				} 
				parent = parent.getParent();
			}
		}
		return link;
	}
	
	
	/**
	 * Given a messageQName and element has this WSDLMessage defined
	 * look for WSDLMessage object.
	 * @param messageQName port type QName
	 * @param element bpel element from where WSDLMessage to be looked up.
	 * @return WSDLMessage object
	 */
	public static Message getWSDLMessage(QName messageQName, BPELElement element) {
		if(messageQName == null) {
			return null;
		}
		
		BPELDocument document = (BPELDocument) element.getOwnerDocument();
		
		if (document == null) {
			return null;
		}
		
		BPELProcess process = document.getDocumentProcess();
				
		if (process == null) {
			return null;
		}
        
		Message message = null;
		
		message = process.getWSDLMessage(messageQName);
		
		
		
		return message;
	
	}
	
	/**
	 * Given a portTypeQName and element look for portType object.
	 * @param portTypeQName port type QName
	 * @param element bpel element from where this portType to be located
	 * @return PortType object
	 */
	public static PortType getWSDLPortType(QName portTypeQName, BPELElement element) {
		if(portTypeQName == null) {
			return null;
		}
		
		BPELDocument document = (BPELDocument) element.getOwnerDocument();
		
		if (document == null) {
			return null;
		}
		
		BPELProcess process = document.getDocumentProcess();
				
		if (process == null) {
			return null;
		}
        
		PortType portType = process.getPortType(portTypeQName);

                return portType;
		
	}
	
	/**
	 * Given a elementQName and element look for ElementDecl object.
	 * @param elementQName port type QName
	 * @param element bpel element from where ElementDecl to be looked 
	 * @return SchemaGlobalElement object
	 */
	public static SchemaGlobalElement getXSDElement(QName elementQName, BPELElement element) {
		if(elementQName == null) {
			return null;
		}
		
		BPELDocument document = (BPELDocument) element.getOwnerDocument();
		
		if (document == null) {
			return null;
		}
		
		BPELProcess process = document.getDocumentProcess();
				
		if (process == null) {
			return null;
		}
        
		SchemaGlobalElement xsdElement = null;
		
		xsdElement = process.getXSDElement(elementQName);
		
		return xsdElement;
		
	}
	
	
	/**
	 * Given a typeQName and element look for XMLType object.
	 * @param typeQName port type QName
	 * @param element bpel element from where to look for XMLType
	 * @return SchemaType object
	 */
	public static SchemaType getXSDType(QName typeQName, BPELElement element) {
		if(typeQName == null) {
			return null;
		}
		
		BPELDocument document = (BPELDocument) element.getOwnerDocument();
		
		if (document == null) {
			return null;
		}
		
		BPELProcess process = document.getDocumentProcess();
				
		if (process == null) {
			return null;
		}
        
		SchemaType type = process.getXSDType(typeQName);
		
		return type;
		
	}
	
	public static Operation getFirstMatchingWSDLOperation(PortType portType, String operationName) {
		if(portType == null || operationName == null) {
			return null;
		}
		
		return portType.getOperation(operationName, null, null);
		
	}
	
	/**
	 * return matching partnerLinkType from a partnerLink
	 * @param partner partnerLink
	 * @return PartnerLinkType
	 */
	public static PartnerLinkType getMatchingPartnerLinkType(PartnerLink partner) {
		PartnerLinkType serviceLinkType = null;
		QName serviceLinkTypeQName = partner.getPartnerLinkType();
		if(serviceLinkTypeQName != null) {
			 if(serviceLinkTypeQName != null) {
				 BPELDocument document = (BPELDocument) partner.getOwnerDocument();
				 BPELProcess process = document.getDocumentProcess();
				 if(process != null) {
					 String wsdlTargetNamespace = serviceLinkTypeQName.getNamespaceURI();
					 Collection wsdls = process.getImportedWSDLDefinitions(wsdlTargetNamespace);
					 Iterator it = wsdls.iterator();
					 while(it.hasNext()) {
						 Definition wsdlDefinition = (Definition) it.next();
						 PartnerLinkType slt =
						     WSDL4JExt.getPartnerLinkType(wsdlDefinition, serviceLinkTypeQName);
						 if(slt != null) {
							 serviceLinkType = slt;
							 break;
						 }
					 }
				 }
			 }
		}
		
		return serviceLinkType;
	}
	
	public static Collection getAllBPELReceive(BPELProcessOrScope process) {
		if(process == null) {
			return Collections.EMPTY_LIST;
		}
		return getAllContainedReceive(process);
		
	}
	
	
	private static Collection getAllContainedReceive(XMLNode node) {
		if(node == null) {
			return Collections.EMPTY_LIST;
		}
		ArrayList receiveList = new ArrayList();
		
		List children = node.getChildren();
		if(children != null) {
			Iterator it = children.iterator();
			
			while(it.hasNext()) {
				XMLNode child = (XMLNode) it.next();
				if(child instanceof Receive) {
					receiveList.add(child);
				} else {
					receiveList.addAll(getAllContainedReceive(child));
				}
			}
		}
		
		return receiveList;
		
	}
	
	public static Collection getAllBPELOnMessage(BPELProcessOrScope process) {
		if(process == null) {
			return Collections.EMPTY_LIST;
		}
		return getAllContainedOnMessage(process);
		
	}
	
	private static Collection getAllContainedOnMessage(XMLNode node) {
		if(node == null) {
			return Collections.EMPTY_LIST;
		}
		ArrayList receiveList = new ArrayList();
		
		List children = node.getChildren();
		if(children != null) {
			Iterator it = children.iterator();
			
			while(it.hasNext()) {
				XMLNode child = (XMLNode) it.next();
				if(child instanceof OnMessage) {
					receiveList.add(child);
				} else {
					receiveList.addAll(getAllContainedOnMessage(child));
				}
			}
		}
		
		return receiveList;
		
	}
	
	/**
     * Get a collection of all bpel receive activities
     * used in this process which have given partnerLink. 
     * This will find out all the
     * nested receives as well.
     * @return Collection of bpel receive activities.
     */
    public static Collection getAllMatchingBPELReceive(String partnerLinkName, BPELProcessOrScope processOrScope) {
    	if(partnerLinkName == null || processOrScope == null) {
    		return Collections.EMPTY_LIST;
    	}
    	ArrayList receiveMatchingPartnerLink = new ArrayList();
    	
    	
    	Collection allReceiveList = processOrScope.getAllBPELReceive();
    	Iterator it = allReceiveList.iterator();
    	
    	while(it.hasNext()) {
    		Receive receive = (Receive) it.next();
    		String pLinkName = receive.getPartnerLink();
    		if(partnerLinkName.equals(pLinkName)) {
    			receiveMatchingPartnerLink.add(receive);
    		}
    	}
    	
    	return receiveMatchingPartnerLink;
    }
    
    /**
     * Get a collection of all bpel onMessage activities
     * used in this process which have given partnerLinkName. 
     * This will find out all the
     * nested onMessage as well.
     * @return Collection of bpel onMessage activities.
     */
    public static Collection getAllMatchingBPELOnMessage(String partnerLinkName,  BPELProcessOrScope processOrScope) {
    	if(partnerLinkName == null) {
    		return Collections.EMPTY_LIST;
    	}
    	ArrayList onMessageMatchingPartnerLink = new ArrayList();
    	
    	
    	Collection allReceiveList = processOrScope.getAllBPELOnMessage();
    	Iterator it = allReceiveList.iterator();
    	
    	while(it.hasNext()) {
    		OnMessage onMessage = (OnMessage) it.next();
    		String pLinkName = onMessage.getPartnerLink();
    		if(partnerLinkName.equals(pLinkName)) {
    			onMessageMatchingPartnerLink.add(onMessage);
    		}
    	}
    	
    	return onMessageMatchingPartnerLink;
    }
    
    
    /**
     * Get the partner corresponding to the partner name
     * @param partnerName The name of the partner
     * @return PartnerLink The partner object
     */
    public static PartnerLink getPartner(String partnerName, PartnerLinks partnerLinks) {
        
        if (partnerName == null 
        	|| partnerName.trim().equals("") 
        	|| partnerLinks == null) {
            return null;
        }
        
        for (int i = 0; i < partnerLinks.getPartnerLinksSize(); i++) {
            PartnerLink partner = partnerLinks.getPartnerLink(i);
            
            if (partnerName.equals(partner.getName())) {
                return partner;
            }

            
        }

        return null;
    }

    /**
     * Get the partner corresponding to the partner name
     * @param partnerName The name of the partner
     * @return PartnerLink The partner object
     */
    public static Variable getVariable(String variableName, Variables variables) {
        
        if (variableName == null 
        	|| variableName.trim().equals("") 
        	|| variables == null) {
            return null;
        }
        
        for (int i = 0; i < variables.getVariableSize(); i++) {
            Variable variable = variables.getVariable(i);
            
            if (variableName.equals(variable.getName())) {
                return variable;
            }
            
        }

        return null;
    }
     

	 /** Tests if an attribute value is absent.
     * @param   value   Value of attribute.
     * @return  <code>true</code> if value is absent.
     */
    public static boolean isValueAbsent(String value) {
        return ((null == value) || (value.trim().length() == 0));
    }
    
}
