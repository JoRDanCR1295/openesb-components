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
 * @(#)DebugLog.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.impl;

import java.io.StringWriter;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProvider;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;


/**
 * This class may not be Multi-thread safe
 *
 * @author Sun Microsystems
 */
public class DebugLog {
    /** DOCUMENT ME! */

    /**
     * Creates a new instance of DebugLog
     */
    public DebugLog() {
    }

    /**
     * creats debug log
     * 
     * @param aLogger
     *            Logger
     * @param msg
     *            log message
     * @param doc
     *            Document
     */
    public static synchronized void debugLog(Logger aLogger, String msg,
            Document doc) {
        if (aLogger.isLoggable(Level.FINEST)) {
            if (doc != null) {
                debugLog(aLogger, msg, doc.getDocumentElement());
            } else {
                aLogger.log(Level.WARNING, "document_is_null");
            }
        }
    }

    /**
     * creates debug log
     * 
     * @param aLogger
     *            Logger
     * @param msg
     *            log message
     * @param src
     *            Source
     */
    public static synchronized void debugLog(Logger aLogger, String msg, Source src) {
        Level logLevel = Level.FINEST;
        if (!aLogger.isLoggable(logLevel)) {
            return;
        }

        if (src == null) {
            aLogger.log(logLevel, msg);
            return;
        }

        try {
            StringWriter writer = new StringWriter();
            StreamResult dest = new StreamResult(writer);

            XmlResourceProviderPool xmlResProviderpool 
                    = (XmlResourceProviderPool)BPELSERegistry.getInstance()
                            .lookup(XmlResourceProviderPool.class.getName());                    
            XmlResourceProvider xmlResourceProvider = xmlResProviderpool.acquireXmlResourceProvider();
            Transformer transformer = xmlResourceProvider.getTransformer();

            try {
                transformer.transform(src, dest);
            } catch (TransformerException ex) {
                throw ex;
            } finally {
                transformer = null;
                xmlResProviderpool.releaseXmlResourceProvider(xmlResourceProvider);
                xmlResourceProvider = null;                    
            }

            String s = msg + ":\n" + writer.toString();
            aLogger.log(logLevel, s);
        } catch (Exception ex) {
            aLogger.log(Level.WARNING,
                    "Exception occured while writing the log.", ex);
        }
    }

    /**
     * creates debug log
     * 
     * @param aLogger
     *            Logger
     * @param msg
     *            log message
     * @param elem
     *            dom element
     */
    public static synchronized void debugLog(Logger aLogger, String msg,
            Element elem) {
        if (aLogger.isLoggable(Level.FINEST)) {
            try {
                if (elem != null) {
                    DOMSource src = new DOMSource(elem);
                    debugLog(aLogger, msg, src);
                }
            } catch (Exception ex) {
                aLogger.log(Level.WARNING,
                        "Exception occured while writing the log.", ex);
            }
        }
    }
}
