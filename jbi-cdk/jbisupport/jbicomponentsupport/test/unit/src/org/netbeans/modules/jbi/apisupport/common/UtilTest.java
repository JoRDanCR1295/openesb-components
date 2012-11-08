/*
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the License). You may not use this file except in
 * compliance with the License.
 *
 * You can obtain a copy of the License at http://www.netbeans.org/cddl.html
 * or http://www.netbeans.org/cddl.txt.
 * 
 * When distributing Covered Code, include this CDDL Header Notice in each file
 * and include the License file at http://www.netbeans.org/cddl.txt.
 * If applicable, add the following below the CDDL Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 */


package org.netbeans.modules.jbi.apisupport.common;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.io.StringWriter;
import junit.framework.*;

/**
 *
 * @author chikkala
 */
public class UtilTest extends TestCase {
    
    public UtilTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
    }
    
    protected void tearDown() throws Exception {
    }
    
    private void printLibs(String classPath) {
        String[] libArray = Util.getLibraryList(classPath);
        System.out.println("Number of Libs: " + libArray.length);
        for ( int i=0; i < libArray.length; ++i) {
            System.out.println(libArray[i]);
        }
    }
    
    public void testGetLibraryList() throws Exception {
        String classPath = "D:\\xyz\\1.jar:D:\\123\\2.jar;D:/aaa/3.jar:D:/bbb/4.jar";
        printLibs("component.jar:" + classPath);
        classPath = "D:";
        printLibs("component.jar:" + classPath);
        classPath = "D:\\xyz\\1.jar:D:\\";
        printLibs("component.jar:" + classPath);
        classPath = ".;..:../xyz/1.jar:../2.jar;../";
        printLibs("component.jar:" + classPath);
        classPath = ".:..";
        printLibs("component.jar:" + classPath);                
    }
    
}
