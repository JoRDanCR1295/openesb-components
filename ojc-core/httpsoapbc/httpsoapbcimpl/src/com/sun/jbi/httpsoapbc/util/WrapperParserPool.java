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
 * @(#)WrapperParserPool.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.util;

import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;

import java.util.LinkedList;

public class WrapperParserPool {
    private final LinkedList<WrapperParser> mParsers;

    /** Creates a newTrWrapperParserPoolansformerPool */
    public WrapperParserPool() {
        mParsers = new LinkedList<WrapperParser>();
    }
    
    public WrapperParserPool(int size) throws WrapperProcessingException {
        this();
        for (int i = 0; i < size; ++i) {
            mParsers.addFirst(HelperFactory.createParser());
        }
    }
    
    public WrapperParser retrieve() throws WrapperProcessingException {
        WrapperParser parser = null;
        
        synchronized(this) {
            if (!mParsers.isEmpty()) {
                parser = mParsers.removeFirst();
            } else {
                parser = HelperFactory.createParser();
            }
        }
        return parser;
    }
    
    public boolean relinquish(WrapperParser parser) {
        boolean success = false;
        if (parser != null) {
            synchronized (this) {
                if (!mParsers.contains(parser)) {
                    mParsers.addFirst(parser);
                    success = true;
                }
            }
        }
        return success;
    }
}
