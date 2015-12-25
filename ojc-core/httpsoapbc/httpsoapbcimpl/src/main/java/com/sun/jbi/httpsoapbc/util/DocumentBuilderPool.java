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
 * @(#)DocumentBuilderPool.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.util;

import java.util.LinkedList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

public class DocumentBuilderPool {
    private static final DocumentBuilderFactory mBuilderFactory =
            DocumentBuilderFactory.newInstance();
    
    private final LinkedList<DocumentBuilder> mDocumentBuilders;

    /** Creates a new instance of DocumentBuilderPool */
    public DocumentBuilderPool() {
    	mBuilderFactory.setNamespaceAware(true);
        mDocumentBuilders = new LinkedList<DocumentBuilder>();
    }
    
    public DocumentBuilderPool(int size) throws ParserConfigurationException {
        this();
        for (int i = 0; i < size; ++i) {
            mDocumentBuilders.addFirst(mBuilderFactory.newDocumentBuilder());
        }
    }
    
    public DocumentBuilder retrieve() throws ParserConfigurationException {
        DocumentBuilder documentBuilder = null;
        
        synchronized(this) {
            if (!mDocumentBuilders.isEmpty()) {
                documentBuilder = mDocumentBuilders.removeFirst();
            } else {
                documentBuilder  = mBuilderFactory.newDocumentBuilder();
            }
        }
        return documentBuilder;
    }
    
    public boolean relinquish(DocumentBuilder documentBuilder) {
        boolean success = false;
        if (documentBuilder != null) {
            synchronized (this) {
                if (!mDocumentBuilders.contains(documentBuilder)) {
                    mDocumentBuilders.addFirst(documentBuilder);
                    success = true;
                }
            }
        }
        return success;
    }
}
