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
 * @(#)StreamUtilTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp.io.streaming;

import com.sun.jbi.ftpbc.ftp.io.stream.StreamUtil;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/*
 * JUnit based test.
 *
 * 
 * 
 * 
 * @author Harry Liu (harry.liu@sun.com)
 *
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.
 */
public class StreamUtilTest extends TestCase {
    
    public StreamUtilTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
    }
    
    protected void tearDown() throws Exception {
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(StreamUtilTest.class);
        
        return suite;
    }
    
    /**
     * Test of writeToStream method, of class com.sun.jbi.ftpbc.streaming.StreamUtil.
     */
    public void testWriteToStream() throws Exception {
        System.out.println("writeToStream");
        
        byte[] array = "test data for a byte array".getBytes();
        OutputStream os = new ByteArrayOutputStream();
        StreamUtil.writeToStream(array, os);
        assertEquals(new String(array), os.toString());
        
    }
    
    /**
     * Test of copyStream method, of class com.sun.jbi.ftpbc.streaming.StreamUtil.
     */
    public void testCopyStream() throws Exception {
        System.out.println("copyStream");
        
        String testData = "source for input stream";
        InputStream source = new ByteArrayInputStream(testData.getBytes());
        OutputStream dest = new ByteArrayOutputStream();
        int bufferSize = 8192;
        StreamUtil.copyStream(source, dest, bufferSize);
        assertEquals(testData, dest.toString());
        
        // smaller buffer
        bufferSize = 3;
        StreamUtil.copyStream(source, dest, bufferSize);
        assertEquals(testData, dest.toString());
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
