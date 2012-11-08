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

package com.sun.jbi.httpsoapbc.util;

import java.io.InputStream;
import java.io.Reader;
import java.io.StringWriter;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.xml.soap.SOAPElement;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/** This class may not be Multi-thread safe
 *
 */
public class DebugLog {
    
    private static final TransformerPool cTransformerPool = 
            new TransformerPool();
    
    /** Creates a new instance of DebugLog */
    public DebugLog() {
    }
    
    /**
     * Beware, this may use synchronization when logging is enabled and may slow processing
     */
    public static void debugLog(Logger aLogger, Level logLevel, String msg, Document doc) {
        if (aLogger != null) {
            if (doc != null) {
                debugLog(aLogger, logLevel, msg, doc.getDocumentElement());
            } else {
                if (aLogger.isLoggable(logLevel)) {
                    aLogger.log(logLevel, msg);
                }
            }
        }
    }

    /**
     * Beware, this may use synchronization when logging is enabled and may slow processing
     */    
    public static void debugLog(Logger aLogger, Level logLevel, String msg, Source src) {

        if (aLogger != null && aLogger.isLoggable(logLevel) && src != null) {        
            Transformer transformer = null;
            try {
                StringWriter writer = new StringWriter();
                StreamResult dest = new StreamResult(writer);

                transformer = cTransformerPool.retrieve();

                if (src instanceof StreamSource) {
                    StreamSource stream = (StreamSource)src;
                    InputStream inputStream = stream.getInputStream();
                    if (inputStream != null) {
                        inputStream.reset();
                    }
                    Reader reader = stream.getReader();
                    if (reader != null) {
                        reader.reset();
                    }
                }
                transformer.transform(src, dest);
                if (src instanceof StreamSource) {
                    StreamSource stream = (StreamSource)src;
                    InputStream inputStream = stream.getInputStream();
                    if (inputStream != null) {
                        inputStream.reset();
                    }
                    Reader reader = stream.getReader();
                    if (reader != null) {
                        reader.reset();
                    }
                }
                String s = msg + ":\n" + writer.toString();
                aLogger.log(logLevel, s);
            } catch (Exception ex) {
                ex.printStackTrace();
            } finally {
                cTransformerPool.relinquish(transformer);
            }
        }
    }

    /**
     * Beware, this may use synchronization when logging is enabled and may slow processing
     */    
    public static void debugLog(Logger aLogger, Level logLevel, String msg, Element elem) {
        if (aLogger != null && aLogger.isLoggable(logLevel)) {
            try {
                if (elem != null) {
                    DOMSource src = new DOMSource(elem);
                    debugLog(aLogger, logLevel, msg, src);
                } 
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }

    public static void debugLog(Logger aLogger, Level logLevel, String msg, java.util.Map map) {
        if (aLogger != null && aLogger.isLoggable(logLevel)) {
            StringBuffer strBuff = new StringBuffer();
            if (map != null) {
                java.util.Set keys = map.keySet();
                java.util.Iterator iter = keys.iterator();
                while (iter.hasNext()) {
                    Object key = iter.next();
                    Object val = map.get(key);
                    strBuff.append(key).append('=').append(val).append('\n');
                }
            }
            strBuff.insert(0, '{');
            strBuff.insert(strBuff.length(), '}');
            strBuff.insert(0, msg + ":\n");
            aLogger.log(logLevel, strBuff.toString());
        }
    }

}
