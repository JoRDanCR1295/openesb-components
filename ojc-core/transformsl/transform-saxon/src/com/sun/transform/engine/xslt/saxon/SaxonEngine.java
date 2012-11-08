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
 * @(#)SaxonEngine.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.xslt.saxon;

import javax.jbi.messaging.ExchangeStatus;
import javax.xml.transform.dom.DOMSource;

import net.sf.saxon.s9api.DOMDestination;
import net.sf.saxon.s9api.Processor;
import net.sf.saxon.s9api.QName;
import net.sf.saxon.s9api.SaxonApiException;
import net.sf.saxon.s9api.XdmAtomicValue;
import net.sf.saxon.s9api.XdmValue;
import net.sf.saxon.s9api.XsltTransformer;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.sun.jbi.common.xml.XmlUtil;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.transform.engine.model.Transform;
import com.sun.transform.engine.runtime.ProcessingException;
import com.sun.transform.engine.runtime.impl.Invoker;
import com.sun.transform.engine.xslt.AbstractTransformEngine;

/**
 * Saxon implementation of transformation engine.
 * @author Kevan Simpson
 */
public class SaxonEngine extends AbstractTransformEngine<XsltTransformer, Transform<XsltTransformer>> {
    private static final Processor mProcessor = new Processor(false);
    
    /** No-arg constructor for instantiation via reflection. */
    public SaxonEngine() {
    }

    /**
     * @param ctx
     */
    public SaxonEngine(ManagerContext ctx) {
        super(ctx);
    }

    /** @see com.sun.transform.engine.xslt.AbstractTransformEngine#setParameter(java.lang.Object, java.lang.String, java.lang.Object) */
    @Override
    protected void setParameter(XsltTransformer xsl, String param, Object value) 
            throws ProcessingException {
        try {
            XdmValue xdmValue = null;
            
            if (value instanceof ExchangeStatus || value instanceof String) {
                xdmValue = new XdmAtomicValue(String.valueOf(value));
            }
            else if (value instanceof Node) {
                xdmValue = mProcessor.newDocumentBuilder()
                        .build(new DOMSource((Node) value));
            }
            else if (value instanceof Invoker) {
                /*
                 * Forced to access javax.xml.transform.Transformer implementation
                 * provided by Saxon to set Object parameter
                 */
                xsl.getUnderlyingController().setParameter(param, value);
                return;
            }

            xsl.setParameter(QName.fromClarkName(param), xdmValue);
        }
        catch (SaxonApiException sae) {
            throw transformError(sae);
        }
    }

    /** @see com.sun.transform.engine.xslt.AbstractTransformEngine#transform(java.lang.Object, javax.xml.transform.dom.DOMSource) */
    @Override
    protected Element transform(XsltTransformer xsl, DOMSource src)
            throws ProcessingException {
        try {
            Document domResult = XmlUtil.newDocument();
            DOMDestination result = new DOMDestination(domResult);
            xsl.setSource(src);
            xsl.setDestination(result);
            xsl.transform();
            return domResult.getDocumentElement();
        }
        catch (SaxonApiException sae) {
            throw transformError(sae);
        }
    }
}
