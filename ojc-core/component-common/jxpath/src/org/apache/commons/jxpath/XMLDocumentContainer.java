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
 * @(#)XMLDocumentContainer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

import java.net.URL;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;

import org.apache.commons.jxpath.xml.DocumentContainer;
import org.w3c.dom.Document;

/**
 * An XML document container reads and parses XML only when it is
 * accessed.  JXPath traverses Containers transparently -
 * you use the same paths to access objects in containers as you
 * do to access those objects directly.  You can create
 * XMLDocumentContainers for various XML documents that may or
 * may not be accessed by XPaths.  If they are, they will be automatically
 * read, parsed and traversed. If they are not - they won't be
 * read at all.
 *
 * @deprecated 1.1 Please use org.apache.commons.jxpath.xml.DocumentContainer
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class XMLDocumentContainer implements Container {

    private DocumentContainer delegate;
    private Object document;
    private URL xmlURL;
    private Source source;
    private String parser;

    /**
     * @param  URL is a URL for an XML file. Use getClass().getResource
     * (resourceName) to load XML from a resource file.
     */
    public XMLDocumentContainer(URL xmlURL) {
        delegate = new DocumentContainer(xmlURL);
    }

    public XMLDocumentContainer(Source source) {
        this.source = source;
        if (source == null) {
            throw new RuntimeException("Source is null");
        }
    }

    /**
     * Reads XML, caches it internally and returns the Document.
     */
    public Object getValue() {
        if (document == null) {
            try {
                if (source != null) {
                    DOMResult result = new DOMResult();
                    Transformer trans =
                        TransformerFactory.newInstance().newTransformer();
                    trans.transform(source, result);
                    document = (Document) result.getNode();
                }
                else {
                    document = delegate.getValue();
                }
            }
            catch (Exception ex) {
                throw new JXPathException(
                    "Cannot read XML from: "
                        + (xmlURL != null
                            ? xmlURL.toString()
                            : (source != null
                                ? source.getSystemId()
                                : "<<undefined source>>")),
                    ex);
            }
        }
        return document;
    }

    /**
     * Throws an UnsupportedOperationException
     */
    public void setValue(Object value) {
        throw new UnsupportedOperationException();
    }
}
