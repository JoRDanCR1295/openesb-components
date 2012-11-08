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
 * @(#)FastSchemaFactoryImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xsd.model.impl;

import java.io.File;
import java.io.InputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import com.sun.xsd.model.FastSchema;
import com.sun.xsd.model.FastSchemaFactory;



/**
 * @author Sun Microsystems
 *
 * A factory which parses wsdl fast. 
 * Just parse some attributes from wsdl and ignore rests.
 */
public class FastSchemaFactoryImpl extends FastSchemaFactory {
	
	private boolean mParseImports = false;
	
	
	private Logger logger = Logger.getLogger(this.getClass().getName());
	
	public FastSchema newFastSchema(InputStream in, boolean parseImports) {
		this.mParseImports = parseImports;
		FastSchema def = new FastSchemaImpl();
		try {
			SAXParserFactory fac = SAXParserFactory.newInstance();
			SAXParser parser = fac.newSAXParser();
			FastWSDLDefinitionsHandler handler = new FastWSDLDefinitionsHandler(def);
			parser.parse(in, handler);
			
		} catch(Exception ex) {
			logger.log(Level.SEVERE, "Failed to parse wsdl", ex);
			def.setParseErrorMessage(ex.getMessage());
		}
		
		return def;
	}
	 
	public FastSchema newFastSchema(String defFileUrl) {
		return newFastSchema(defFileUrl, false);
	}
	
	public FastSchema newFastSchema(String defFileUrl, 
													  boolean parseImports) {
		this.mParseImports = parseImports;
		FastSchema def = new FastSchemaImpl();
		
		File file = new File(defFileUrl);
		
		try {
			SAXParserFactory fac = SAXParserFactory.newInstance();
			SAXParser parser = fac.newSAXParser();
			FastWSDLDefinitionsHandler handler = new FastWSDLDefinitionsHandler(def);
			parser.parse(file, handler);
			
		} catch(Exception ex) {
			logger.log(Level.SEVERE, "Failed to parse "+ defFileUrl, ex);
			def.setParseErrorMessage(ex.getMessage());
		}
		
		return def;
	}
	
	public class FastWSDLDefinitionsHandler extends DefaultHandler {
		
		private String targetNamespace;
		
		private FastSchema mDef;
		
		public FastWSDLDefinitionsHandler(FastSchema def) {
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
		 	if(qName.endsWith("schema")) {
		 		for(int i = 0 ; i < attributes.getLength(); i++) {
		 			String attrQName = attributes.getQName(i); 
		 			if(attrQName.endsWith("targetNamespace")) {
		 				targetNamespace = attributes.getValue(i);
		 				mDef.setTargetNamespace(targetNamespace);
		 				break;
		 			}
		 		}
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
