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
 * @(#)JDOMParser.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.xml;

import java.io.InputStream;

import org.apache.commons.jxpath.JXPathException;
import org.jdom.input.SAXBuilder;

/**
 * An implementation of the XMLParser interface that produces a JDOM Document.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class JDOMParser extends XMLParser2 {
    
    public Object parseXML(InputStream stream) {
        if (!isNamespaceAware()) {
            throw new JXPathException("JDOM parser configuration error. JDOM "
                    + "does not support the namespaceAware=false setting.");
        }
        
        try {
            SAXBuilder builder = new SAXBuilder();
            builder.setExpandEntities(isExpandEntityReferences());
            builder.setIgnoringElementContentWhitespace(
                    isIgnoringElementContentWhitespace());
            builder.setValidation(isValidating());
            return builder.build(stream);
        }
        catch (Exception ex) {
            throw new JXPathException("JDOM parser error", ex);
        }
    }
}
