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
 * @(#)TransformerPool.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.util;

import java.util.LinkedList;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;

public class TransformerPool {
    private static final TransformerFactory cTransformerFact =
            TransformerFactory.newInstance();
    
    private final LinkedList<Transformer> mTransformers;

    /** Creates a new instance of TransformerPool */
    public TransformerPool() {
        mTransformers = new LinkedList<Transformer>();
    }
    
    public TransformerPool(int size) throws TransformerConfigurationException {
        this();
        for (int i = 0; i < size; ++i) {
            mTransformers.addFirst(cTransformerFact.newTransformer());
        }
    }
    
    public Transformer retrieve() throws TransformerConfigurationException {
        Transformer transformer = null;
        
        synchronized(this) {
            if (!mTransformers.isEmpty()) {
                transformer = mTransformers.removeFirst();
            } else {
                transformer = cTransformerFact.newTransformer();
            }
        }
        return transformer;
    }
    
    public boolean relinquish(Transformer transformer) {
        boolean success = false;
        if (transformer != null) {
            synchronized (this) {
                if (!mTransformers.contains(transformer)) {
                    mTransformers.addFirst(transformer);
                    success = true;
                }
            }
        }
        return success;
    }
}
