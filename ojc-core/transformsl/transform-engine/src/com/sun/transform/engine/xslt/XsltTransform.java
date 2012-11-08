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
 * @(#)XsltTransform.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.engine.xslt;

import java.io.File;

import javax.jbi.management.DeploymentException;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamSource;

import com.sun.transform.I18n;
import com.sun.transform.engine.model.impl.TransformImpl;
import com.sun.transform.engine.runtime.ProcessingException;

/**
 * XSLT-specific implementation of a transformation activity model,
 * containing a compiled form of the XSL stylesheet.
 * 
 * @author Kevan Simpson
 */
public class XsltTransform extends TransformImpl<Transformer> {
	private static TransformerFactory mTransformerFactory = 
			TransformerFactory.newInstance();
	
	private Templates mTemplates;
	
	/**
	 * Constructs an XSLT-specific transformation activity model.
	 * @param file The location of the XSL stylesheet to compile.
	 * @param source The variable reference of transformation input.
	 * @param result The variable reference of transformation output.
	 */
	public XsltTransform(String name, String file, String source, String result) {
		super(name, file, source, result);
	}

	/** @see com.sun.transform.engine.model.Transform#compile(java.lang.String) */
    public void compile(String rootPath) throws DeploymentException {
        // check file exists...
        if (verifyFileExists(rootPath, this.getFile())) {
            // ...then compile
            mTemplates = getTemplates(rootPath, this.getFile());
        }
    }

    /**
	 * Returns a <code>Transformer</code> representing the compiled stylesheet.
	 * @return a <code>Transformer</code>.
	 * @throws ProcessingException if the stylesheet cannot be compiled.
	 */
    public Transformer newTransformer() throws ProcessingException {
        try {
            // identity transform, no stylesheet file specified
            if (mTemplates == null) {
                synchronized (mTransformerFactory) {
                	// not thread-safe, hence synchronize
                    return mTransformerFactory.newTransformer(); 
                }
            }
            // Templates implementations are thread-safe
            return mTemplates.newTransformer();
        }
        catch (TransformerConfigurationException tce) {
            throw new ProcessingException(I18n.loc(
                    "TRANSL-6049: Failed to create new Transformer: {0}", tce.getMessage()),
                    tce);
        }
    }
    
    private Templates getTemplates(String rootPath, String fileName) 
            throws DeploymentException {
        Templates ret = null;
        try {
            synchronized (mTransformerFactory) {
                ret = mTransformerFactory.newTemplates(new StreamSource(
                        new File(rootPath, fileName)));
            }
        } 
        catch (TransformerConfigurationException tce) {
            throw new DeploymentException(I18n.loc(
                    "TRANSL-6047: Failed to compile stylesheet - {0}: {1}", 
                            fileName, tce.getMessage()),
                    tce);
        }

        return ret;
    }
}
