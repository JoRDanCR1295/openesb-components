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
 * @(#)FileInputStreamTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp.io.streaming;

import com.sun.jbi.ftpbc.ftp.io.stream.FileInputStream;
import java.io.File;
import java.io.FileDescriptor;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
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
public class FileInputStreamTest extends TestCase {
    File testFile;
    FileInputStream instance;
    
    public FileInputStreamTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        // create a file with 10 bytes content
        testFile = File.createTempFile("testFileInputStream","");
        FileOutputStream fos = new FileOutputStream(testFile);
        byte[] tenBytes = "0123456789".getBytes();
        fos.write(tenBytes);
        fos.flush();
        fos.close();
        
        instance = new FileInputStream(testFile);
    }
    
    protected void tearDown() throws Exception {
        try {
            testFile.deleteOnExit();
            testFile.delete();
            instance.close();
        } catch (Exception e) {
            ;
        }
        instance = null;
        super.tearDown();
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(FileInputStreamTest.class);
        
        return suite;
    }
    
    /**
     * Test of mark method, of class com.sun.jbi.ftpbc.streaming.FileInputStream.
     */
    public void testMark() throws Exception {
        System.out.println("mark");
        
        int readlimit = 0;
        
        instance.mark(readlimit);
        
    }
    
    /**
     * Test of read method, of class com.sun.jbi.ftpbc.streaming.FileInputStream.
     */
    public void testRead() throws Exception {
        System.out.println("read");
        
        int expResult = '0';
        int result = instance.read();
        assertEquals(expResult, result);
        assertEquals(1, instance.getPosition());
        
        // read again
        expResult = '1';
        result = instance.read();
        assertEquals(expResult, result);
        assertEquals(1 + 1, instance.getPosition());
        
        // read again
        expResult = '2';
        result = instance.read();
        assertEquals(expResult, result);
        assertEquals(1 + 1 + 1, instance.getPosition());
        
        // read again
        expResult = '3';
        result = instance.read();
        assertEquals(expResult, result);
        assertEquals(1 + 1 + 1 + 1, instance.getPosition());
        
    }
    
    /**
     * Test of read method, of class com.sun.jbi.ftpbc.streaming.FileInputStream.
     */
    public void testRead_b() throws Exception {
        System.out.println("read_b");
        
        byte[] b = new byte[4];
        int expResult = b.length;
        
        int result = instance.read(b);
        assertEquals(expResult, result);
        assertEquals(b.length, instance.getPosition());
        
        // read again
        result = instance.read(b);
        assertEquals(expResult, result);
        assertEquals(b.length + b.length, instance.getPosition());
        
        // read again ==> exceeds 10 bytes ==> reaches end
        result = instance.read(b);
        assertEquals(true, instance.endOfFile());
        assertEquals(2, result);
        assertEquals(10, instance.getPosition());
        
        // read again after reaches end
        result = instance.read(b);
        assertEquals(true, instance.endOfFile());
        assertEquals(-1, result);
        assertEquals(10, instance.getPosition());
        
    }
    
    /**
     * Test of read method, of class com.sun.jbi.ftpbc.streaming.FileInputStream.
     */
    public void testRead_b_off_len() throws Exception {
        System.out.println("read_b_off_len");
        
        byte[] b = new byte[4];
        int off = 1;
        int len = 2;
        int expResult = off + len - 1;
        
        int result = instance.read(b, off, len);
        assertEquals(expResult, result);
        assertEquals(off + len - 1, instance.getPosition());
        
        // read again
        result = instance.read(b, off, len);
        assertEquals(expResult, result);
        assertEquals((off + len - 1) + (off + len - 1), instance.getPosition());
        
    }
    
    /**
     * Test of reset method, of class com.sun.jbi.ftpbc.streaming.FileInputStream.
     */
    public void testReset() throws Exception {
        System.out.println("reset");
        
        try {
            instance.reset();
            fail("An exception is expected - not supported by the super class java.io.FileInputStream.");
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        
    }
    
    /**
     * Test of skip method, of class com.sun.jbi.ftpbc.streaming.FileInputStream.
     */
    public void testSkip() throws Exception {
        System.out.println("skip");
        
        long n = 1;
        long expResult = 1;
        long result = instance.skip(n);
        assertEquals(expResult, result);
        assertEquals(1, instance.getPosition());
        
        // skip 1 byte again
        result = instance.skip(n);
        assertEquals(expResult, result);
        assertEquals(1 + 1, instance.getPosition());
        
        // skip 1 byte again
        result = instance.skip(n);
        assertEquals(expResult, result);
        assertEquals(1 + 1 + 1, instance.getPosition());
        
        // skip 1 byte again
        result = instance.skip(n);
        assertEquals(expResult, result);
        assertEquals(1 + 1 + 1 + 1, instance.getPosition());
        
        // skip 20 bytes again ==> exceeds 10 bytes ==> reaches end
        n = 20;
        result = instance.skip(n);
        assertEquals(true, instance.endOfFile());
        //?why not like read(b)? assertEquals(6, result); //? upon super class java.io.FileInputStream
        //?why not like read(b)? assertEquals(10, instance.getPosition());  //? upon super class java.io.FileInputStream
        assertEquals(n, result);  //? upon super class java.io.FileInputStream
        assertEquals(4 + n, instance.getPosition());  //? upon super class java.io.FileInputStream
        
        // skip 1 byte again after reaches end
        result = instance.skip(1);
        assertEquals(true, instance.endOfFile());
        //?why not like read(b)? assertEquals(-1, result); //? upon super class java.io.FileInputStream
        //?why not like read(b)? assertEquals(10, instance.getPosition()); //? upon super class java.io.FileInputStream
        assertEquals(1, result); //? upon super class java.io.FileInputStream
        assertEquals(4 + n + 1, instance.getPosition()); //? upon super class java.io.FileInputStream
        
    }
    
    /**
     * Test of endOfFile method, of class com.sun.jbi.ftpbc.streaming.FileInputStream.
     */
    public void testEndOfFile() throws Exception {
        System.out.println("endOfFile");
        
        boolean expResult = false;
        boolean result = instance.endOfFile();
        assertEquals(expResult, result);
        
        instance.skip(100);
        assertEquals(true, instance.endOfFile());
        
    }
    
    /**
     * Test of getPosition method, of class com.sun.jbi.ftpbc.streaming.FileInputStream.
     */
    public void testGetPosition() throws Exception {
        System.out.println("getPosition");
        
        long expResult = 0;
        long result = instance.getPosition();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of getSize method, of class com.sun.jbi.ftpbc.streaming.FileInputStream.
     */
    public void testGetSize() throws Exception {
        System.out.println("getSize");
        
        long expResult = 10;
        long result = instance.getSize();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of main method, of class com.sun.jbi.ftpbc.streaming.FileInputStream.
     */
    public void testMain() throws Exception {
        System.out.println("main");
        
        String[] args = new String[1];
        args[0] = testFile.getPath();
        
        FileInputStream.main(args);
        
    }
    
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
