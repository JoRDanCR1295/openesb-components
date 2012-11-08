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
 * @(#)HelperFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.nms.wsdl11wrapper;

import com.sun.jbi.nms.wsdl11wrapper.impl.WrapperParserImpl;
import com.sun.jbi.nms.wsdl11wrapper.impl.WrapperBuilderImpl;

/**
 * Factory to help with building and parsing the JBI WSDL 1.1 Wrapper normalized messages
 * @author Sun Microsystems
 */
public class HelperFactory {

    /**
     * Temporary switch for the WSDL 1.1 JBI wrapper.
     * false = use old custom SBYN wrapper.
     * true = use JBI standard wrapper
     *
     * NOTE THAT THIS CONSTANT WILL BE REMOVED once the migratino to the new wrapper is done.
     */
    public static final boolean WRAPPER_ENABLED = true;

    /**
     * Temporary switch for including the part name element in the WSDL 1.1 JBI
     * wrapper for message parts defined as "type"
     *
     * false = use the "correct" wrapper without the part name element
     * true = use the "backwards compatible" wrapper which includes the part name element
     *
     * NOTE THAT THIS SWITCH WILL BE REMOVED once the migratino to the new wrapper is done.
     */
    public static final boolean WRAPPER_TYPE_INCLUDE_PARTNAME = false;

    /** Disallow instantiating this factory */
    private HelperFactory() {
    }

    /**
     * Factory method,
     * create a parser to assist in processing a JBI WSDL 1.1 wrapped normalized message
     *
     * @return parser
     * @throws WrapperException if a parser could not be created
     */
    public static WrapperParser createParser() throws WrapperProcessingException {
        return new WrapperParserImpl();
    }

    /**
     * Factory method,
     * create a builder to assist in building a JBI WSDL 1.1 wrapped normalized message
     * @return builder
     * @throws WrapperException if a builder could not be created
     */
    public static WrapperBuilder createBuilder() throws WrapperProcessingException {
        return new WrapperBuilderImpl();
    }
}
