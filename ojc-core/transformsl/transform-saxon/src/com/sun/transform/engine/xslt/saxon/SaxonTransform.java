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
 * @(#)SaxonTransform.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.xslt.saxon;

import java.io.File;

import javax.jbi.management.DeploymentException;
import javax.xml.transform.stream.StreamSource;

import net.sf.saxon.Controller;
import net.sf.saxon.TransformerFactoryImpl;
import net.sf.saxon.s9api.Processor;
import net.sf.saxon.s9api.SaxonApiException;
import net.sf.saxon.s9api.XsltCompiler;
import net.sf.saxon.s9api.XsltExecutable;
import net.sf.saxon.s9api.XsltTransformer;

import com.sun.transform.api.saxon.I18n;
import com.sun.transform.engine.model.impl.TransformImpl;
import com.sun.transform.engine.runtime.ProcessingException;

/**
 * Saxon-B implementation of a transformation activity model,
 * containing a compiled form of the XSL stylesheet.
 * 
 * @author Kevan Simpson
 */
public class SaxonTransform extends TransformImpl<XsltTransformer> {
    private static final Processor mProcessor = new Processor(false);
    private static final TransformerFactoryImpl mFactory =
            new TransformerFactoryImpl(mProcessor.getUnderlyingConfiguration());
    
    private XsltExecutable mXsltExec;
    
    /**
     * @param name
     * @param file
     * @param source
     * @param result
     */
    public SaxonTransform(String name, String file, String source, String result) {
        super(name, file, source, result);
    }

    /** @see com.sun.transform.engine.model.Transform#compile(java.lang.String) */
    public void compile(String rootPath) throws DeploymentException {
        try {
            XsltCompiler compiler = mProcessor.newXsltCompiler();
            mXsltExec = compiler.compile(
                    new StreamSource(new File(rootPath, this.getFile())));
        }
        catch (SaxonApiException sae) {
            throw new DeploymentException(I18n.loc(
                    "TRASAX-6001: Failed to compile stylesheet - {0}: {1}", 
                            this.getFile(), sae.getMessage()),
                    sae);
        }
    }

    public XsltTransformer newTransformer() throws ProcessingException {
        if (mXsltExec == null) {
            // identity transform, no stylesheet file specified
            try {
                Controller cont = (Controller)
                        mFactory.newTransformer();
                return new EmptyTransformer(cont);
            }
            catch (Exception e) {
                throw new ProcessingException(I18n.loc(
                        "TRASAX-6002: Failed to create XsltTransformer for {0}: {1}", 
                        this, e.getMessage()), e);
            }
        }
        return mXsltExec.load();
    }
    
    private static class EmptyTransformer extends XsltTransformer {
        public EmptyTransformer(Controller cont) {
            super(mProcessor, cont);
        }
    }
}
