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
 * @(#)Utility.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.common;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.StringTokenizer;

import javax.xml.parsers.DocumentBuilder;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProvider;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;


/**
 * @author Sun Microsystems
 */
public class Utility {
    public static void assertArrayEquals(String msg, Object[] a, Object[] b) {
        if (!Arrays.equals(a, b)) {
            TestCase.fail(msg + " Arrays are not equal");
        }
    }

    public static void assertArrayEquals(String msg, long[] a, long[] b) {
        if (!Arrays.equals(a, b)) {
            TestCase.fail(msg + " Arrays are not equal");
        }
    }

    public static void logEnter(String className, String methodName) {
        System.out.println("Entering " + className + "_" + methodName);
    }

    public static void logExit(String className, String methodName) {
        System.out.println("Exiting " + className + "_" + methodName);
    }

    public static Element createDOMElement(InputSource is) {
        try {
            XmlResourceProviderPool pool = (XmlResourceProviderPool)
                    BPELSERegistry.getInstance().lookup(
                            XmlResourceProviderPool.class.getName());
            XmlResourceProvider rsrc = pool.acquireXmlResourceProvider();
            DocumentBuilder db = rsrc.getDocumentBuilder();
            Document doc = null;
            try { doc = db.parse(is); }
            finally { pool.releaseXmlResourceProvider(rsrc); }

            if (doc == null) {
                throw new IllegalArgumentException("xml parse error: ");
            }

            return doc.getDocumentElement();
        } catch (Exception e) {
            throw new RuntimeException("parse xml error");
        }
    }
    
    public static Properties loadProperties(File propertyFile) throws Exception {
    	Properties props = new Properties ();
    	FileInputStream fileIo = null;
    	try {
    		fileIo = new FileInputStream(propertyFile);
    		props.load(new BufferedInputStream(fileIo));
    	}catch (Exception e) {
    		e.printStackTrace();
    		throw e;
    	}finally {
    		if (fileIo != null) {    			
    			fileIo.close();
    		}
    	}
    	return props;
    	
    }
    
    public static List parseString(String toParse, String deli) {
        List tokens = null;
        if (toParse != null && toParse.trim().length() > 0) {
            StringTokenizer stringToken = new StringTokenizer(toParse, deli);
            tokens = new ArrayList();
            while (stringToken.hasMoreTokens()) {
                String tokenStr = stringToken.nextToken().trim();
                while (tokenStr.endsWith("+")) {
                    tokenStr = tokenStr.substring(0, tokenStr.length()-1) + "," + stringToken.nextToken().trim();
                }
                tokens.add(tokenStr);
            }
        }
        return tokens;
    }
    
    public static Properties loadProperties(String fileName) throws Exception {
        InputStream is = Utility.class.getResourceAsStream(fileName);
        Properties props = new Properties();
        props.load(is);
        return props;
    }
}
