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
 * @(#)SaxonTransformer.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.transform.api.saxon;

import java.util.HashMap;
import java.util.Map;
import javax.xml.transform.Source;
import net.sf.saxon.s9api.DOMDestination;
import net.sf.saxon.s9api.QName;
import net.sf.saxon.s9api.XsltTransformer;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.transform.api.Xslt2Support.Xslt2;
import com.sun.transform.api.saxon.ext.SaxonExec;

/**
 * Saxon-B implementation of {@link Xslt2}.
 * @author Kevan Simpson
 */
public class SaxonTransformer implements Xslt2 {
    private ExecutablePool mPool;
    private Map<QName, Object> mParams;
    
    public SaxonTransformer(ExecutablePool pool) {
        mPool = pool;
        mParams = new HashMap<QName, Object>();
    }
    
    /** @see com.sun.transform.api.Xslt2Support.Xslt2#setParam(java.lang.String, java.lang.Object) */
    public void setParam(String name, Object param) throws Exception {
        mParams.put(QName.fromClarkName(name), param);//convertXdmParam(param));
    }

    /** @see com.sun.transform.api.Xslt2Support.Xslt2#doTransform(javax.xml.transform.Source) */
    public Element doTransform(Source payload) throws Exception {
        Document doc = XmlUtil.newDocument();
        DOMDestination result = new DOMDestination(doc);

        SaxonExec exec = mPool.acquire();
        try {
            XsltTransformer sax = exec.load();
            sax.setSource(payload);
            if (!mParams.isEmpty()) {
                for (QName nm : mParams.keySet()) {
                    sax.setParameter(nm, exec.convertXdmParam(mParams.get(nm)));
                }
            }
            sax.setDestination(result);
            sax.transform();
            return doc.getDocumentElement();
        }
        finally {
            mPool.release(exec);
        }
    }
}
