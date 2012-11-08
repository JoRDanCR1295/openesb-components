/*
 * @(#)WSDLConstants.java        $Revision: 1.4 $ $Date: 2008/12/17 23:24:43 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.shared.wsdl;

import javax.xml.namespace.QName;

/**
 * Global constants used for generation and processing of the WSDL
 * that describes the deployed ruleset.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.4 $ $Date: 2008/12/17 23:24:43 $
 * 
 * @since 0.1
 */
public final class WSDLConstants {

    public static final String XML_SCHEMA_NAMESPACE_URI = "http://www.w3.org/2001/XMLSchema";
    
    public static final String TYPES_NAMESPACE_URI = "http://www.milanfort.com/xml/ns/jbi/rules/types";

    public static final String INPUT_MESSAGE_LOCAL_NAME = "InputMessage";

    public static final String OUTPUT_MESSAGE_LOCAL_NAME = "OutputMessage";
    
    public static final String INPUT_ELEMENT_NAME = "InputData";
    
    public static final String OUTPUT_ELEMENT_NAME = "OutputData";
    
    public static final QName INPUT_ELEMENT_QUALIFIED_NAME = new QName(TYPES_NAMESPACE_URI, INPUT_ELEMENT_NAME);
    
    public static final QName OUTPUT_ELEMENT_QUALIFIED_NAME = new QName(TYPES_NAMESPACE_URI, OUTPUT_ELEMENT_NAME);
    
    private WSDLConstants() {}
}
