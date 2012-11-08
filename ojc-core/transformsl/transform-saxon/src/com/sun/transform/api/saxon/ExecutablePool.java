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
 * @(#)ExecutablePool.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.api.saxon;

import java.io.InputStream;
import java.util.logging.Level;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import net.sf.saxon.s9api.Processor;
import com.sun.jbi.common.util.AbstractPool;
import com.sun.transform.api.Xslt2Support.CompiledXslt2;
import com.sun.transform.api.Xslt2Support.Xslt2;
import com.sun.transform.api.saxon.ext.SaxonCompiler;
import com.sun.transform.api.saxon.ext.SaxonExec;

/**
 * Saxon-B implementation of {@link CompiledXslt2}.
 * @author Kevan Simpson
 */
public class ExecutablePool extends AbstractPool<SaxonExec> implements CompiledXslt2 {
    private Source mStylesheet;
    
    /**
     */
    public ExecutablePool(Source stylesheet) {
        super(10, false);
        mStylesheet = stylesheet;
        init(); // after setting stylesheet, making it available in createResource()
    }

    /** @see com.sun.transform.api.Xslt2Support.CompiledXslt2#cleanup() */
    public void cleanup() {
        super.cleanup();    // from AbstractPool
        if (mStylesheet instanceof StreamSource) {
            InputStream stream = ((StreamSource) mStylesheet).getInputStream();
            if (stream != null) {
                try {
                    stream.close();
                }
                catch (Exception e) {
                    log().log(Level.FINE, I18n.loc(
                            "TRASAX-3001: Error closing stylesheet input stream: {0}", 
                            e.getMessage()), 
                            e);
                }
            }
        }
    }

    /** @see com.sun.transform.api.Xslt2Support.CompiledXslt2#newTransformer() */
    public Xslt2 newTransformer() throws Exception {
        return new SaxonTransformer(this);
    }

    /** @see com.sun.jbi.common.util.AbstractPool#createResource() */
    @Override
    protected SaxonExec createResource() {
        try {
            Processor proc = new Processor(false);
            SaxonCompiler comp = new SaxonCompiler(proc);

            if (log().isLoggable(Level.CONFIG)) {
                log().config(I18n.loc(
                        "TRASAX-4001: Creating new pooled XSL resource: {0}", 
                        String.valueOf(comp)));
            }
            
            return (SaxonExec) comp.compile(mStylesheet);  
        }
        catch (Exception x) {
            return null;
        }
    }
}
