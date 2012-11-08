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
 * @(#)SaxonProcessFactory.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.xslt.saxon;

import net.sf.saxon.s9api.XsltTransformer;
import com.sun.transform.engine.model.ProcessFactory;
import com.sun.transform.engine.model.Transform;
import com.sun.transform.engine.model.impl.ProcessFactoryImpl;

/**
 * Saxon-B implementation of a {@link ProcessFactory}.
 * @author Kevan Simpson
 */
public class SaxonProcessFactory extends ProcessFactoryImpl {
    public Transform<XsltTransformer> createTransform(String name, String file, 
                                                      String inputPart, 
                                                      String outputPart) {
        return new SaxonTransform(name, file, inputPart, outputPart);
    }
}
