#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://${package}.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://${package}.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(${symbol_pound})FileOutputTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.extensions;

import junit.framework.*;

/**
 *
 * @author sweng
 */
public class FileOutputTest extends TestCase {

    FileOutput instance = new FileOutput();

    public FileOutputTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(FileOutputTest.class);

        return suite;
    }

    /**
     * Test of setFileMessage and getFileMessage method, of class com.sun.jbi.filebc.extensions.FileInput.
     */
    public void testSetGetFileMessage() {
        System.out.println("Testing setFileRead and getFileRead");

        FileMessage fileMessage = new FileMessage();
        instance.setFileMessage(fileMessage);
        FileMessage result = instance.getFileMessage();
        assertEquals(fileMessage, result);

        System.out.println("Successfully tested setFileRead and getFileRead");
    }
}
