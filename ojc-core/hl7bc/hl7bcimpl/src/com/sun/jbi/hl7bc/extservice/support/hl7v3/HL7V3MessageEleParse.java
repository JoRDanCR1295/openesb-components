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
 * @(#)HL7V3MessageEleParse.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.support.hl7v3;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.events.StartElement;
import javax.xml.stream.events.XMLEvent;
import javax.xml.transform.Source;
import javax.xml.namespace.QName;
import com.sun.jbi.hl7bc.I18n;

/**
 * This class parses xml instance, creates context, populate it with the
 * needed processing attributes
 * 
 * @author S. Nageswara Rao, Raghunadh
 * 
 */
public class HL7V3MessageEleParse {

	private static XMLInputFactory xmlInputFactory;

	private static XMLEventReader xmlEventReader;

	private static final Logger log = Logger.getLogger(HL7V3MessageEleParse.class.getName());

	private static final String ID = "id";

	private static final String CREATION_TIME = "creationTime";

	private static final String INTERACTIONID = "interactionId";

	private static final String VALUE = "value";

	private static final String ROOT = "root";

	private static final String EXTENSION = "extension";

	private static final String PROCESSINGCODE = "processingCode";

	private static final String ACCEPTACKCODE = "acceptAckCode";

	private static final String VERSIONCODE = "versionCode";

	private static final String SEQUENCENUMBER = "sequenceNumber";

	private static final String CODE = "code";

	private static final String CONTROLACTPROCESS = "controlActProcess";

	static {
		try {
			xmlInputFactory = XMLInputFactory.newInstance();
			// Requires the processor to replace internal entity references with
			// their
			// replacement value and report them as characters or the set of
			// events that describe
			// the entity
			xmlInputFactory.setProperty(
					XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES,
					Boolean.TRUE);
			// Requires the processor to resolve external parsed entities
			xmlInputFactory.setProperty(
					XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES,
					Boolean.FALSE);
			xmlInputFactory.setProperty(XMLInputFactory.IS_NAMESPACE_AWARE,
					Boolean.TRUE);
			// Requires the processor to coalesce adjacent character data
			xmlInputFactory.setProperty(XMLInputFactory.IS_COALESCING,
					Boolean.TRUE);
		} catch (javax.xml.stream.FactoryConfigurationError ex) {
			log.log(Level.SEVERE,
					I18n.msg("An instance of the XMLInput factory cannot be loaded"),
					new Object[] { ex });
		}
	}

	/**
	 * initializes the event reader with the xml instance
	 * 
	 * @param source
	 */
	public static void initEventReader(Source source) {
		try {
			xmlEventReader = xmlInputFactory.createFilteredReader(
					xmlInputFactory.createXMLEventReader(source),
					new javax.xml.stream.EventFilter() {
						public boolean accept(XMLEvent reader) {
							if (!reader.isStartElement())
								return false;
							else
								return true;
						}
					});
		} catch (javax.xml.stream.XMLStreamException xmlStExc) {
			log
					.log(
							Level.SEVERE,
							I18n.msg("Unexpected exception occured while creating the XMLEvent Reader, the reasion is {0}",
								xmlStExc ));
		}
	}

	/**
	 * This method can be called to create and populates the transmission
	 * wrapper context with the required message processing data
	 * 
	 * @return HL7V3TransmissionWrapperContext
	 */
	public static HL7V3TransmissionWrapperContext getTransmissionWrapperContext() throws Exception {
		HL7V3TransmissionWrapperContext tWContext = new HL7V3TransmissionWrapperContext();
		XMLEvent xmlEvent = null;
		StartElement stElement = null;
		String code = null, controlID = null, value = null, extension = null;
		// iterate over the events
		while (xmlEventReader.hasNext()) {
			xmlEvent = xmlEventReader.nextEvent();
			if (xmlEvent.isStartElement()) {
				stElement = xmlEvent.asStartElement();
				if (stElement.getName().getLocalPart().equalsIgnoreCase(ID)) {
					controlID = stElement.getAttributeByName(new QName(ROOT))
							.getValue();
					tWContext.setId(controlID);
				} else if (stElement.getName().getLocalPart().equalsIgnoreCase(
						CREATION_TIME)) {
					value = stElement.getAttributeByName(new QName(VALUE))
							.getValue();
					tWContext.setCreationTime(value);
				} else if (stElement.getName().getLocalPart().equalsIgnoreCase(
						INTERACTIONID)) {
					extension = stElement.getAttributeByName(
							new QName(EXTENSION)).getValue();
					tWContext.setInteractionId(extension);
				} else if (stElement.getName().getLocalPart().equalsIgnoreCase(
						PROCESSINGCODE)) {
					code = stElement.getAttributeByName(new QName(CODE))
							.getValue();
					tWContext.setProcessingCode(code);
				} else if (stElement.getName().getLocalPart().equalsIgnoreCase(
						ACCEPTACKCODE)) {
					code = stElement.getAttributeByName(new QName(CODE))
							.getValue();
					tWContext.setAcceptAckCode(code);
				} else if (stElement.getName().getLocalPart().equalsIgnoreCase(
						VERSIONCODE)) {
					code = stElement.getAttributeByName(new QName(CODE))
							.getValue();
					tWContext.setVersionCode(code);
				} else if (stElement.getName().getLocalPart().equalsIgnoreCase(
						SEQUENCENUMBER)) {
					value = stElement.getAttributeByName(new QName(VALUE))
							.getValue();
					tWContext.setSequenceNumber(new Integer(value).intValue());
				} else if (stElement.getName().getLocalPart().equalsIgnoreCase(
						CONTROLACTPROCESS)) {
					// stop further processing
					break;

				}
			}
		}
		return tWContext;
	}

}
