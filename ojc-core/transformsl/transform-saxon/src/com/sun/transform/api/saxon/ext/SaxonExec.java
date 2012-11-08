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
 * @(#)SaxonExec.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.api.saxon.ext;

import javax.xml.transform.dom.DOMSource;
import net.sf.saxon.PreparedStylesheet;
import net.sf.saxon.s9api.Processor;
import net.sf.saxon.s9api.SaxonApiException;
import net.sf.saxon.s9api.XdmAtomicValue;
import net.sf.saxon.s9api.XdmValue;
import net.sf.saxon.s9api.XsltExecutable;
import org.w3c.dom.Node;

/**
 * Extension of Saxon's <code>XsltExecutable</code> to make a pooled
 * <code>Processor</code> available to convert XSL parameters.
 * @author Kevan Simpson
 */
public class SaxonExec extends XsltExecutable {
    private Processor mProc;

    public SaxonExec(Processor proc, PreparedStylesheet pss) {
        super(proc, pss);
        mProc = proc;
    }

    public XdmValue convertXdmParam(Object param) throws SaxonApiException {
        if (param != null) {
            if (param instanceof Node) {
                return mProc.newDocumentBuilder()
                        .build(new DOMSource((Node) param));
            } 
            else {
                return new XdmAtomicValue(String.valueOf(param));
            }
        }

        return null;
    }

//    public Xslt2 load(Source payload) throws SaxonApiException {
//        SaxonTransformer xsl = 
//                new SaxonTransformer(mProc, getUnderlyingCompiledStylesheet());
//        AugmentedSource aug = AugmentedSource.makeAugmentedSource(payload);
//        aug.setWrapDocument(false);
//        xsl.setSource(aug);
//
//        return xsl;
//    }
}
