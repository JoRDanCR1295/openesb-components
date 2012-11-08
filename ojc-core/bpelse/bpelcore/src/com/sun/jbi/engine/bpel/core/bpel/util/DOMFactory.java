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
 * @(#)DOMFactory.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.util;

import javax.xml.namespace.QName;

import org.apache.commons.jxpath.AbstractFactory;
import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.Pointer;
import org.apache.xmlbeans.SchemaField;
import org.apache.xmlbeans.SchemaProperty;
import org.apache.xmlbeans.SchemaType;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 * This implementation of the org.apache.commons.jxpath.AbstractFactory
 * is not schema aware. It assumes that the object(Node) to be created 
 * is a vaild one and does not attempt to validate if its invalid. 
 * @author Sun Microsystems
 *
 */
public class DOMFactory extends AbstractFactory {

	/**
	 * creates DOM object
	 * 
	 * @param context jxpath context
	 * @param pointer node pointer
	 * @param parent parent object
	 * @param name object name
	 * @param index object index
	 * @return boolean: on successful execution, returns true; otherwise, returns false
	 */
	public boolean createObject(JXPathContext context, Pointer pointer, Object parent, String name, int index) {
		addDOMElement(context, (Node) parent, index, name);
		return true;
	}

	/**
	 * declares variable
	 * 
	 * @param context jxpath context
	 * @param name variable name
	 * @return boolean: on successful execution, returns true; otherwise, returns false
	 */
	public boolean declareVariable(JXPathContext context, String name) {
		return false;
	}

	/**
	 * adds DOM element. It is assumed that the object (node) to be added 
	 * is valid and as per schema. 
	 * 
	 * @param parent parent node
	 * @param index index position
	 * @param tag node tag
	 */
	private void addDOMElement(JXPathContext context, Node parent, int index, String tag) {
		String tagToCreate = null;
		String nodeNS = null;
		String nodePrefix = null;
		String queryPrefix = null;
		String nodeLocalName = null;

		int position = tag.indexOf(':');
		Document doc = parent.getOwnerDocument();
		/***************************************************************************************
		 * if queryPrefix is not null, node will be created is either corresponds to global
		 * element or local element with schema defined with elementFormDefault= "unqualified"
		 */
		if (position > 0l) {
			queryPrefix = tag.substring(0, position);
			nodeLocalName = tag.substring(position + 1, tag.length());
			nodeNS = context.getNamespaceURI(queryPrefix);
			if (nodeNS == null) {
				throw new RuntimeException(I18n.loc("BPCOR-6190: Unable to resolve namespace prefix {0}", queryPrefix));
			}
		} else {
			nodeLocalName = tag;
			
			Object schemaType = parent.getUserData("schemaType");
			if (schemaType != null) {
				SchemaType schType = null;
				if (schemaType instanceof SchemaType) {
					schType = (SchemaType)schemaType;
				} else {
					schType = ((SchemaField) schemaType).getType();
				}
				SchemaProperty[] schemaProp = schType.getElementProperties();
				QName chldElem = null;
				/*
				 * Legacy bpel support:
				 * If using a schema that has defined its elementFormDefault="qualified"
				 * and if the corresponding bpel assign <to> expression has not been qualified 
				 * by the namespace prefix, then the namespace prefix is derived by checking
				 * the element in the SchemaType and extracting its namespace. If the element
				 * does not exist in the SchemaType returned then it defaults to the namespace 
				 * of the last iterated element. 
				 */
				for(int i=0; i < schemaProp.length ; i++){
					chldElem = schemaProp[i].getName();
					if(nodeLocalName.equals(chldElem.getLocalPart())){
						break;
					}
					chldElem = null;
				}
				
				if (chldElem == null) {
					throw new RuntimeException(I18n.loc("BPCOR-6030: Unknown element {0}", nodeLocalName));
				}
				
				nodeNS = chldElem.getNamespaceURI();
			}
		}

		if (nodeNS != null) {
			// DEVNOTE VM: Fix for bug 593. Namespace URI should never be an empty string. 'ElementNSImpl' follows this
			// and hence sets its namespace URI to null if it is an empty string. Since getNameSpaceURI() returns
			// null for empty namespace URI, the comparison below fails even though the Nodes are of the same type.
			// For the comparison to pass we treat empty string as null also as per DOM level 3 spec.
			if (nodeNS.length() == 0) {
				nodeNS = null;
				tagToCreate = nodeLocalName;
			} else {
                            nodePrefix = parent.lookupPrefix(nodeNS);
                            // DEVNOTE VM: Fix for 856
                            // Check to see if the namespace uri is the same as the same as the parents. If yes, then
                            // the parent or one of it's ancestors has defined it as their default namespace thereby not
                            // requiring a prefix. In that case, we will also not define a prefix.

                            //this check is done early so that we can avoid prefixes
                            if (parent.isDefaultNamespace(nodeNS)) {
                                tagToCreate = nodeLocalName;
                            } else if (nodePrefix == null) {
                                // calculate unique prefix and also transform query prefix to document prefix.
                                // they may not be same
                                int counter = 2;
                                if (queryPrefix == null) {
                                    queryPrefix = DOMHelper.BASE_PREFIX;
                                }
                                nodePrefix = queryPrefix;
                                while (parent.lookupNamespaceURI(nodePrefix) != null) {
                                    nodePrefix = queryPrefix + counter++;
                                }
                                tagToCreate = nodePrefix + ":" + nodeLocalName;
                            } else {
                                tagToCreate = nodePrefix + ":" + nodeLocalName;
                            }
			}
		} else {
			tagToCreate = nodeLocalName;
		}
		
		// check if this node already exists
		Node child = parent.getFirstChild();
		int count = 0;
		while (child != null) {
			String childNodeNS = child.getNamespaceURI();
			String childNodeLocalName = child.getLocalName();
			if (Utility.areEqual(nodeNS, childNodeNS) && Utility.areEqual(nodeLocalName, childNodeLocalName)) {
				count++;
			}
			child = child.getNextSibling();
		}
		
		// Keep inserting new elements until we have index + 1 of them
		while (count <= index) {
			Node newElement = null;
			newElement = doc.createElementNS(nodeNS, tagToCreate);
			parent.appendChild(newElement);
			count++;
		}
	}
}
