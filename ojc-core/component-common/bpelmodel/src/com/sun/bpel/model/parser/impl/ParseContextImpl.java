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
 * @(#)ParseContextImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.parser.impl;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELDocumentFactory;
import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.visitor.DefaultXmlParserFactory;


/**
 * BPEL Parse Context implementation
 *
 * @author Sun Microsystems
 */
public class ParseContextImpl extends BPELParseContext {
    /** singleton for the parseContext */
//    private static BPELParseContext mParseCtx = new ParseContextImpl();

    /**   */
    private RXmlParserFactoryImpl mParseFac = new RXmlParserFactoryImpl();

    /**   */
    private RBPELDocumentFactoryImpl mBPELDocfac = new RBPELDocumentFactoryImpl();
//
//    /**
//     * singleton for the parseContext
//     *
//     * @return bpel parse context
//     */
//    public static BPELParseContext getParseContext() {
//        return mParseCtx;
//    }

    /**
     * gets XML Parser Factory
     *
     * @return DefaultXmlParserFactory default XML parser factory
     */
    public DefaultXmlParserFactory getXMLParserFactory() {
        return mParseFac;
    }

    /**
     * gets BPEL document factory
     *
     * @return BPELDocumntFactory BPEL document factory
     */
    public BPELDocumentFactory getBPELDocumentFactory() {
        return mBPELDocfac;
    }

    private static class RBPELDocumentFactoryImpl extends BPELDocumentFactory {
        /**
         * creates new BPEL document
         *
         * @return BPELDocument BPEL document
         */
        public BPELDocument newBPELDocument() {
            return RBPELDocumentImpl.createBPELDocument();
        }
    }
}
