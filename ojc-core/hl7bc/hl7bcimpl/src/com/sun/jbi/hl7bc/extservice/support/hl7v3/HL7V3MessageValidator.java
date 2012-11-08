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
 * @(#)HL7V3MessageValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.support.hl7v3;

import java.io.File;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.Part;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.jbi.messaging.MessageExchange;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.sun.jbi.hl7bc.Endpoint;
import com.sun.jbi.hl7bc.extensions.HL7Message;
import com.sun.jbi.hl7bc.I18n;

import static com.sun.jbi.hl7bc.packaging.WSDLConfigurations.*;

public class HL7V3MessageValidator {

	private static final Logger mLog = Logger.getLogger(HL7V3MessageValidator.class.getName());

	private static DocumentBuilder db = null;

	static {
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		dbf.setNamespaceAware(true);
		try {
			db = dbf.newDocumentBuilder();
		} catch (Exception exc) {
			mLog.log(Level.SEVERE,
					I18n.msg("Unable to create document builder, the reason is {0}",
					exc ));
		}
	}

	public static boolean validateMsg(QName operationName, Endpoint endpoint,
			HL7Message hl7Message, String data) throws Exception {
		Service service = endpoint.getDefinition().getService(
				endpoint.getServiceName());
		Port port = service.getPort(QName.valueOf(endpoint.getEndpointName())
				.getLocalPart());
		PortType portType = port.getBinding().getPortType();
		Message wsdlMessage = null;
		for (Iterator it = portType.getOperations().iterator(); it.hasNext();) {
			Operation op = (Operation) it.next();
			if (op.getName().equals(operationName.toString())
					|| op.getName().equals(operationName.getLocalPart())) {
				wsdlMessage = op.getInput().getMessage();
				break;
			}
		}

		Part aPart = null;
		QName messageQName = wsdlMessage.getQName();
		String partName = hl7Message.getPart();

		if (partName != null && !partName.equals("")) {
			aPart = wsdlMessage.getPart(partName);
			if (aPart == null) {
				throw new Exception(I18n.msg(
						"Failed to locate message part for part name {0}.", partName));
			}
		} else {
			// there is only one part
			Collection parts = wsdlMessage.getParts().values();
			Part[] partArray = (Part[]) parts.toArray(new Part[0]);
			aPart = (Part) partArray[0];
		}
		//String nameSpaceURI = aPart.getElementName().getNamespaceURI();
		String xsdFileLoc = getXsdFileLocation(aPart.getElementName(), endpoint.getXsdsList());
		if (xsdFileLoc != null) {
			Document document = db.parse(new InputSource(data));
			Schema schema = null;
			synchronized (HL7V3MessageValidator.class) {
				// create a SchemaFactory capable of understanding WXS schemas
				SchemaFactory factory = SchemaFactory
						.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);

				// load a WXS schema, represented by a Schema instance
				Source schemaFile = new StreamSource(new File(xsdFileLoc));
				schema = factory.newSchema(schemaFile);
			}
			if (schema != null) {
				synchronized (HL7V3MessageValidator.class) {
					// create a Validator instance, which can be used to validate an instance document
					Validator validator = schema.newValidator();
					// validate the DOM tree
					try {
						validator.validate(new DOMSource(document));
					} catch (SAXException saxExc) {
						mLog.log(Level.SEVERE,
								I18n.msg("XML Instance document is invalid. Validation is failed, the reason is {0}",
									 saxExc ));
						return false;
					}
				}
			}
			return true;
		}
		return false;
	}

}
