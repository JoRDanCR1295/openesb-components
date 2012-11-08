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

package com.sun.encoder.hl7.runtime.provider;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 *
 * @author sun
 */
interface Marshaler {

    Marshaler startElement(String uri, String localName, String qName,
        Attributes atts) throws SAXException;

    Marshaler endElement(String uri, String localName, String qName)
        throws SAXException;

    void characters(char[] ch, int start, int length)
        throws SAXException;
}
