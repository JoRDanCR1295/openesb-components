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
 * @(#)XsltEngine.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.xslt;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.jbi.common.xml.XmlUtil;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.transform.engine.model.Transform;
import com.sun.transform.engine.runtime.ProcessingException;

/**
 * Default implementation of an XSLT engine.
 * @author Kevan Simpson
 */
public class XsltEngine 
        extends AbstractTransformEngine<Transformer, Transform<Transformer>> {
    /** No-arg constructor for instantiation via reflection. */
    public XsltEngine() {
    }

    public XsltEngine(ManagerContext ctx) {
		super(ctx);
	}

    /** @see com.sun.transform.engine.xslt.AbstractTransformEngine#setParameter(java.lang.Object, java.lang.String, java.lang.Object) */
    @Override
    protected void setParameter(Transformer xsl, String param, Object value) 
            throws ProcessingException{
        xsl.setParameter(param, value);
    }

    /** @see com.sun.transform.engine.xslt.AbstractTransformEngine#transform(java.lang.Object, javax.xml.transform.dom.DOMSource) */
    @Override
    protected Element transform(Transformer xsl, DOMSource src) 
            throws ProcessingException {
        try {
            DOMResult result = new DOMResult(XmlUtil.newDocument());
            xsl.transform(src, result);
            return ((Document) result.getNode()).getDocumentElement();
        }
        catch (TransformerException te) {
            throw transformError(te);
        }
    }
}
