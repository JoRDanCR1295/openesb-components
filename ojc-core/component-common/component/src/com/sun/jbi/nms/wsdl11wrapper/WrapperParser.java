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
 * @(#)WrapperParser.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.nms.wsdl11wrapper;

import java.util.List;
import javax.wsdl.Message;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

/**
 * Assist in processing normalized messages with JBI WSDL 1.1 wrappers
 *
 * Usage sequence: for each normalized message to process call parse() before using the
 * get() accessors to obtain details regarding the normalized message.
 *
 * e.g. along the lines of
 * parse(normalizedMsg, wsdlDef)
 * getPartElement(partName1)
 * getPartElement(partName2)
 *
 * The same instance should not be used by multiple threads concurrently as it is
 * not guaranteed to be thread safe - and maintains state
 *
 * The re-use of the same instance however is encouraged.
 *
 * @author Sun Microsystems
 */
public interface WrapperParser {

    /**
     * Parse a normalized message document in JBI WSDL 1.1 wrapper format
     * @param wrappedDoc the wrapped normalized message document
     * @param wsdlDefinition the full wsdl definition, including the definition for the normalized message
     * @throws WrapperProcessingException if there is an issue parsing the normalized message,
     * e.g. if the normalized message could not be found in the WSDL
     */
    void parse(Document wrappedDoc, Definition wsdlDefinition) throws WrapperProcessingException;

    /**
     * Parse a normalized message document in JBI WSDL 1.1 wrapper format
     * @param wrappedDoc the wrapped normalized message document
     * @param wsdlMessageDefinition the wsdl message definition for the normalized message
     * @throws WrapperProcessingException if there is an issue parsing the normalized message,
     * e.g. if the normalized message does not match the WSDL description
     */
    void parse(Document wrappedDoc, Message wsdlMessageDefinition) throws WrapperProcessingException;

    /**
     * Note that the spec mandates that all parts present in the WSDL message definition
     * have to appear in the wrapped normalized message - as such this method has limited usefulness
     *
     * @param partName the name of the part to check
     * @return true if the parsed document contains an entry for the given part - even if the part payload itself is empty.
     * false if the part is not present
     */
    boolean hasPart(String partName);

    /**
     * Returns only the first Element inside the part wrapper
     * Legally, the jbi:part may contain multiple Elements, or text - but
     * in many cases the WSDL will limit this to one element.
     *
     * (a jbi:part element may legally contain multiple Elements, or text)
     * @param partName the name of the part
     * @return the first Element in the normalized message part, null if no element is present
     * @throws WrapperProcessingException if the part could not be returned
     */
/*
    Element getPartElement(String partName) throws WrapperProcessingException;
*/
    /**
     * Returns all nodes inside the part wrapper
     * (a jbi:part element may legally contain multiple Elements, or a text node)
     * @param partName the name of the part
     * @return all Nodes in the normalized message part
     * @throws WrapperProcessingException if the part could not be returned
     */
    NodeList getPartNodes(String partName) throws WrapperProcessingException;

    /**
     * Returns the part, including the <jbi:part> part wrapper element around
     * the part 'payload'
     *
     * (a jbi:part element may legally contain multiple Elements, or text)
     * @param partName the name of the part
     * @return the wrapped normalized message part
     * @throws WrapperProcessingException if the part could not be returned
     */
    Element getWrappedPart(String partName) throws WrapperProcessingException;

    /**
     * @return the number of parts in the normalized message
     */
    int getNoOfParts();

    /**
     * @return the names of the parts in the normalized message
     */
    String[] getPartNames();

    /**
     * Get the message type of the message wrapper in the wrapped document.
     * @return the message type as QName
     */
    QName getMessageType() throws WrapperProcessingException;

    /**
     * Get the optional message "name" defined in the wrapped document.
     * This is the logical name defined in the operation binding, not the type name.
     * @return the message name
     */
    String getMessageName() throws WrapperProcessingException;

    /**
     * @return true if the message passed to the parse() methods has a WSDL 1.1 wrapper
     * element, false if it does not
     */ 
    boolean isMessageWrapped() throws WrapperProcessingException;
    
}
