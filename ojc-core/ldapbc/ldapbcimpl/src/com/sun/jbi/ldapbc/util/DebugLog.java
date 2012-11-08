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

package com.sun.jbi.ldapbc.util;

import java.io.StringWriter;
import java.util.logging.Logger;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.w3c.dom.Element;


/** This class may not be Multi-thread safe
 *
 *  DebugLog
 */
public class DebugLog {
    static Transformer transformer = null;

    /** Creates a new instance of DebugLog */
    public DebugLog() {
    }

    public static synchronized void debugLog(final Logger aLogger, final String msg,
        final Document doc) {
        if (doc != null) {
            DebugLog.debugLog(aLogger, msg, doc.getDocumentElement());
        } else {
            aLogger.warning("document is null");
        }
    }

    public static synchronized void debugLog(final Logger aLogger, final String msg,
        final Element elem) {
        if (DebugLog.transformer == null) {
            try {
                final TransformerFactory fact = TransformerFactory.newInstance();
                DebugLog.transformer = fact.newTransformer();
            } catch (final TransformerConfigurationException ex) {
                ex.printStackTrace();
            }
        }

        try {
            if ((elem != null) && (aLogger != null)) {
                final DOMSource src = new DOMSource(elem);
                final StringWriter writer = new StringWriter();
                final StreamResult dest = new StreamResult(writer);

                DebugLog.transformer.transform(src, dest);

                final String s = msg + ":\n" + writer.toString();
                aLogger.info(s);
            }
        } catch (final Exception ex) {
            ex.printStackTrace();
        }
    }
}
