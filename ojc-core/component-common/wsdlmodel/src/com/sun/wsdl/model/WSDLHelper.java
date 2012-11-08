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
 * @(#)WSDLHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model;

import org.exolab.castor.xml.schema.ElementDecl;
import org.exolab.castor.xml.schema.XMLType;

import javax.xml.namespace.QName;

/**
 * 
 * @author Sun Microsystems
 *
 */
public class WSDLHelper {
	
	
	/**
	 * Given a xsdTypeQName and element which has this type defined
	 * look for XMLType object.
	 * @param xsdTypeQName xsd type QName
	 * @param element xml element which has xsd type attribute
	 * @return XMLType object
	 */
	public static XMLType getMatchingXSDType(QName xsdTypeQName, WSDLElement element) {
		if(xsdTypeQName == null) {
			return null;
		}
		
		WSDLDocument document = (WSDLDocument) element.getOwnerDocument();
		
		if (document == null) {
			return null;
		}
		
		WSDLDefinitions wsdlDefinition = document.getDocumentDefinitions();
				
		if (wsdlDefinition == null) {
			return null;
		}
        
		XMLType xsdType = null;
		
		//QName nsQName = NamespaceUtility.normalizeQName(xsdTypeQName, element);
		//xsdType = wsdlDefinition.getXSDType(nsQName);
		xsdType = wsdlDefinition.getXSDType(xsdTypeQName);
		return xsdType;
		
	}
	
	
	/**
	 * Given a xsdTypeQName and element which has this type defined
	 * look for XMLType object.
	 * @param xsdTypeQName xsd type QName
	 * @param element xml element which has xsd type attribute
	 * @return XMLType object
	 */
	public static ElementDecl getMatchingXSDElement(QName xsdElementQName, WSDLElement element) {
		if(xsdElementQName == null) {
			return null;
		}
		
		WSDLDocument document = (WSDLDocument) element.getOwnerDocument();
		
		if (document == null) {
			return null;
		}
		
		WSDLDefinitions wsdlDefinition = document.getDocumentDefinitions();
				
		if (wsdlDefinition == null) {
			return null;
		}
        
		ElementDecl xsdElement = null;
		
		//QName nsQName = NamespaceUtility.normalizeQName(xsdElementQName, element);
		//xsdElement = wsdlDefinition.getXSDElement(nsQName);
		xsdElement = wsdlDefinition.getXSDElement(xsdElementQName);
		
		return xsdElement;
		
	}
	

	/**
	 * Given a wsdlMessageQName and element which has this WSDL message defined
	 * look for WSDLMessage object.
	 * @param wsdlMessageQName xsd type QName
	 * @param element xml element which has  message attribute
	 * @return WSDLMessage object
	 */
	public static WSDLMessage getMatchingWSDLMessage(QName wsdlMessageQName, WSDLElement element) {
		if(wsdlMessageQName == null) {
			return null;
		}
		
		WSDLDocument document = (WSDLDocument) element.getOwnerDocument();
		
		if (document == null) {
			return null;
		}
		
		WSDLDefinitions wsdlDefinition = document.getDocumentDefinitions();
				
		if (wsdlDefinition == null) {
			return null;
		}
        
		WSDLMessage wsdlMessage = null;
		
		wsdlMessage = wsdlDefinition.getMessage(wsdlMessageQName);
		return wsdlMessage;
		
	}
	/** Tests if an attribute value is absent.
     * @param   value   Value of attribute.
     * @return  <code>true</code> if value is absent.
     */
    public static boolean isValueAbsent(String value) {
        return ((null == value) || (value.trim().length() == 0));
    }
}
