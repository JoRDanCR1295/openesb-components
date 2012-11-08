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
 * @(#)FastWSDLDefinitionsFactoryImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.impl;

import java.io.File;
import java.io.InputStream;
import java.io.Reader;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import com.sun.wsdl.model.FastWSDLDefinitions;
import com.sun.wsdl.model.FastWSDLDefinitionsFactory;
import com.sun.wsdl.model.Import;
import com.sun.wsdl.model.NamespaceUtility;

import javax.xml.namespace.QName;
import com.sun.wsdl.model.common.visitor.SAXParserSupport;
import com.sun.jbi.internationalization.Messages;

/**
 * @author Sun Microsystems
 *
 * A factory which parses wsdl fast. 
 * Just parse some attributes from wsdl and ignore rests.
 */
public class FastWSDLDefinitionsFactoryImpl extends FastWSDLDefinitionsFactory {
	
	private boolean mParseImports = false;
	
        private static final Messages MESSAGES = 
                Messages.getMessages(FastWSDLDefinitionsFactoryImpl.class);
        private static final Logger LOGGER = 
                Messages.getLogger(FastWSDLDefinitionsFactoryImpl.class);
	
	public FastWSDLDefinitions newFastWSDLDefinitions(InputStream in, boolean parseImports) {
		this.mParseImports = parseImports;
		FastWSDLDefinitionsImpl def = new FastWSDLDefinitionsImpl();
		try {
			SAXParserFactory fac = SAXParserFactory.newInstance();
			SAXParser parser = fac.newSAXParser();
			FastWSDLDefinitionsHandler handler = new FastWSDLDefinitionsHandler(def);
			parser.parse(in, handler);
			
		} catch(Exception ex) {
			LOGGER.log(Level.SEVERE, 
                                MESSAGES.getString(
                                "FastWSDLDefinitionsFactoryImpl.FAILED_TO_PARSE_WSDL"),
                                ex);
			def.setParseErrorMessage(ex.getMessage());
		}
		
		return def;
	}
	
	public FastWSDLDefinitions newFastWSDLDefinitions(Reader in, boolean parseImports) {
		this.mParseImports = parseImports;
		FastWSDLDefinitionsImpl def = new FastWSDLDefinitionsImpl();
		try {
			SAXParserFactory fac = SAXParserFactory.newInstance();
			SAXParser parser = fac.newSAXParser();
			FastWSDLDefinitionsHandler handler = new FastWSDLDefinitionsHandler(def);
			InputSource ins = new InputSource(in);
			parser.parse(ins, handler);
			
		} catch(Exception ex) {
			LOGGER.log(Level.SEVERE, 
                                MESSAGES.getString(
                                "FastWSDLDefinitionsFactoryImpl.FAILED_TO_PARSE_WSDL"),
                                ex);
			def.setParseErrorMessage(ex.getMessage());
		}
		
		return def;
	}
	
	public FastWSDLDefinitions newFastWSDLDefinitions(String defFileUrl) {
		return newFastWSDLDefinitions(defFileUrl, false);
	}
	
	public FastWSDLDefinitions newFastWSDLDefinitions(String defFileUrl, 
													  boolean parseImports) {
		this.mParseImports = parseImports;
		FastWSDLDefinitionsImpl def = new FastWSDLDefinitionsImpl();
		
		File file = new File(defFileUrl);
		
		try {
			SAXParserFactory fac = SAXParserFactory.newInstance();
			SAXParser parser = fac.newSAXParser();
			FastWSDLDefinitionsHandler handler = new FastWSDLDefinitionsHandler(def);
			parser.parse(file, handler);
			
		} catch(Exception ex) {
			LOGGER.log(Level.SEVERE, 
                                MESSAGES.getString(
                                "FastWSDLDefinitionsFactoryImpl.FAILED_TO_PARSE_x_x",
                                new Object[]{ defFileUrl, ex} ));
			def.setParseErrorMessage(ex.getMessage());
		}
		
		return def;
	}
	
	public class FastWSDLDefinitionsHandler extends DefaultHandler {
		
		private String targetNamespace;
		
		private FastWSDLDefinitionsImpl mDef;
		
		public FastWSDLDefinitionsHandler(FastWSDLDefinitionsImpl def) {
			this.mDef = def;
		}
		
		public String getTargetNamespace() {
			return targetNamespace;
		}
		
		public void startElement (String uri, 
		 						   String localName,
								   String qName, 
								   Attributes attributes)
		 				throws SAXException
		 {
			QName wsdlTagQName = NamespaceUtility.getQNameFromURIAndQualifiedString(
							uri, qName);
			//if((MESSAGES.getString("FastWSDLDefinitionsFactoryImpl.DEFINITIONS")).equals(wsdlTagQName.getLocalName())) {
		 	if((MESSAGES.getString("FastWSDLDefinitionsFactoryImpl.DEFINITIONS")).equals(wsdlTagQName.getLocalPart())) {
		 		this.mDef.setWSDL(true);
		 		for(int i = 0 ; i < attributes.getLength(); i++) {
		 			String qnameStr = attributes.getQName(i);
		 			String attrLocalName = NamespaceUtility.getLocalName(qnameStr);
		 			if(attrLocalName.equals(MESSAGES.getString("FastWSDLDefinitionsFactoryImpl.TARGETNAMESPACE"))) {
		 				targetNamespace = attributes.getValue(i);
		 				mDef.setTargetNamespace(targetNamespace);
		 				break;
		 			}
		 		}
		 	//} else if(mParseImports && Import.TAG.equals(wsdlTagQName.getLocalName())) {
		 	} else if(mParseImports && Import.TAG.equals(wsdlTagQName.getLocalPart())) {
		 		Import wsdlImport = this.mDef.createImport();
                wsdlImport.setQualifiedName(qName);
                SAXParserSupport.setAttributes(wsdlImport, attributes);
                mDef.addImport(wsdlImport);
		 	}
		 	
		 	
		 }
		 
		 public void fatalError (SAXParseException e)
			throws SAXException
		    {
		 		
		    }
		 
		 public void error (SAXParseException e)
			throws SAXException
		    {
		 		
		    }
		 
  }
	

}
