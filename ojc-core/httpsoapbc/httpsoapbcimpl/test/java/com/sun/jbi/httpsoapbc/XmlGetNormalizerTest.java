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
 * @(#)XmlGetNormalizerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import junit.framework.*;
import java.util.HashMap;
import java.util.Map;
import javax.jbi.messaging.MessagingException;
import javax.xml.ws.handler.MessageContext;

/**
 *
 * @author nang
 */
public class XmlGetNormalizerTest extends TestCase {
    
    public XmlGetNormalizerTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of extractQueryFromUrl method, of class com.sun.jbi.httpsoapbc.XmlGetNormalizer.
     */
    public void testExtractQueryFromUrl_UrlEncoded() throws Exception {
        System.out.println("extractQueryFromUrl");
        
        String queryString = "n1=v1&n2=v2&n3=v3";
        OperationMetaData opMetaData = new OperationMetaData();
        XmlGetNormalizer instance = new XmlGetNormalizer();
        
        Map<String, String> expResult = new HashMap();
        expResult.put("n1", "v1");
        expResult.put("n2", "v2");
        expResult.put("n3", "v3");
        opMetaData.setHttpUrlEncoding(OperationMetaData.HTTP_URL_ENCODING_ENCODED);
        Map<String, String> result = instance.extractQuery(queryString, opMetaData);
        assertEquals(expResult, result);
        
        
        // variations of input that should still succeed
        queryString = "n1=v1&n2=v2&n3=v3&";
        result = instance.extractQuery(queryString, opMetaData);
        assertEquals(expResult, result);
        
        queryString = "&n1=v1&n2=v2&n3=v3&";
        result = instance.extractQuery(queryString, opMetaData);
        assertEquals(expResult, result);

        queryString = "&&&&&n1=v1&&&&&n2=v2&&n3=v3&&&&&&";
        result = instance.extractQuery(queryString, opMetaData);
        assertEquals(expResult, result);
    }

    /**
     * Test of extractQueryFromUrl method, of class com.sun.jbi.httpsoapbc.XmlGetNormalizer.
     */
    public void testExtractQueryFromUrl_UrlEncoded_UnsupportedEncoding() throws Exception {
        System.out.println("extractQueryFromUrl");
        
        XmlGetNormalizer instance = new XmlGetNormalizer();
        OperationMetaData opMetaData = new OperationMetaData();
        
        try {
            opMetaData.setHttpUrlEncoding(OperationMetaData.HTTP_URL_ENCODING_UNSPECIFIED);
            instance.extractQuery(null, opMetaData);
            fail("Expected raised MessageException due to unspecified url encoding");
        } catch (MessagingException e) {
        }

        try {
            opMetaData.setHttpUrlEncoding(null);
            instance.extractQuery(null, opMetaData);
            fail("Expected raised MessageException due to unspecified (null) url encoding");
        } catch (MessagingException e) {
        }

        try {
            opMetaData.setHttpUrlEncoding("");
            instance.extractQuery(null, opMetaData);
            fail("Expected raised MessageException due to unspecified (blank) url encoding");
        } catch (MessagingException e) {
        }
    }

    /**
     * Test of extractQueryFromUrl method, of class com.sun.jbi.httpsoapbc.XmlGetNormalizer.
     */
    public void testExtractQueryFromUrl_UrlEncoded_NoInput() throws Exception {
        System.out.println("extractQueryFromUrl");
        
        String queryString = "";
        OperationMetaData opMetaData = new OperationMetaData();
        XmlGetNormalizer instance = new XmlGetNormalizer();
        
        opMetaData.setHttpUrlEncoding(OperationMetaData.HTTP_URL_ENCODING_ENCODED);
        Map<String, String> result = instance.extractQuery(queryString, opMetaData);
        assertTrue("Expected empty map from blank url decode process", result.size() == 0);
    }

    /**
     * Test of extractQueryFromUrl method, of class com.sun.jbi.httpsoapbc.XmlGetNormalizer.
     */
    public void testExtractQueryFromUrl_UrlEncoded_Malformed() throws Exception {
        System.out.println("extractQueryFromUrl");
        
        String queryString = "n1=v1&n2";
        OperationMetaData opMetaData = new OperationMetaData();
        XmlGetNormalizer instance = new XmlGetNormalizer();
        
        try {
            opMetaData.setHttpUrlEncoding(OperationMetaData.HTTP_URL_ENCODING_ENCODED);
            Map<String, String> result = instance.extractQuery(queryString, opMetaData);
            fail("Expected raised MessageException due to malformed query string");
        } catch (MessagingException e) {
        }
    }
    
    /**
     * Test of extractQueryFromUrl method, of class com.sun.jbi.httpsoapbc.XmlGetNormalizer.
     */
    public void testExtractQueryFromUrl_UrlReplacement() throws Exception {
        System.out.println("extractQueryFromUrl");
        
        String queryString = "/ox1/yyy23zaaa456";
        OperationMetaData opMetaData = new OperationMetaData();
        XmlGetNormalizer instance = new XmlGetNormalizer();
        
        Map<String, String> expResult = new HashMap();
        expResult.put("n1", "x1");
        expResult.put("n2", "yy23");
        expResult.put("n3", "aaa456");
        opMetaData.setHttpOperationLocation("/o(n1)/y(n2)z(n3)");
        opMetaData.setHttpUrlEncoding(OperationMetaData.HTTP_URL_ENCODING_REPLACEMENT);
        Map<String, String> result = instance.extractQuery(queryString, opMetaData);
        assertEquals(expResult, result);
    }
}
