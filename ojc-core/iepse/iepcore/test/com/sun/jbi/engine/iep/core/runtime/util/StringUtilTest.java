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
 * @(#)StringUtilTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.util;
import junit.framework.*;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.sun.jbi.engine.iep.core.runtime.util.StringUtil;
/*
 * StringUtilTest.java
 * JUnit based test
 *
 * Created on February 15, 2007, 5:10 PM
 */

/**
 *
 * @author rdwivedi
 */
public class StringUtilTest extends TestCase {
    
    public StringUtilTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(StringUtilTest.class);
        return suite;
    }

    /**
     * Test of replacePrefixNotStartWithDot method, of class com.sun.jbi.engine.iep.core.runtime.util.StringUtil.
     */
    public void testReplacePrefixNotStartWithDot() {
        Map replacement = new HashMap();
        replacement.put("a","AA");
        replacement.put("b","BB");
        assertEquals("AA.b.c.d.a_b",StringUtil.replacePrefixNotStartWithDot("a.b.c.d.a_b", replacement));
        assertEquals("apple",StringUtil.replacePrefixNotStartWithDot("apple", replacement));
        
    }

    /**
     * Test of replacePrefix method, of class com.sun.jbi.engine.iep.core.runtime.util.StringUtil.
     */
    public void testReplacePrefix() {
        Map replacement = new HashMap();
        replacement.put("a","AA");
        replacement.put("b","BB");
        assertEquals("AA.BB.c.d.a_b",StringUtil.replacePrefix("a.b.c.d.a_b", replacement));
        assertEquals("apple",StringUtil.replacePrefix("apple", replacement));
        
        
    }

    /**
     * Test of replacePostfix method, of class com.sun.jbi.engine.iep.core.runtime.util.StringUtil.
     */
    public void testReplacePostfix() {
        Map replacement = new HashMap();
        replacement.put("a","AA");
        replacement.put("b","BB");
        assertEquals("a.BB.c.d.a_b",StringUtil.replacePostfix("a.b.c.d.a_b", replacement));
        
    }

    /**
     * Test of replaceWord method, of class com.sun.jbi.engine.iep.core.runtime.util.StringUtil.
     */
    public void testReplaceWord() {
        Map replacement = new HashMap();
        replacement.put("a","AA");
        replacement.put("b","BB");
        assertEquals("AA.BB.c.d.AA.BB",StringUtil.replaceWord("a.b.c.d.a.b", replacement));
        assertEquals("same",StringUtil.replaceWord("same", replacement));
        
        
    }

    /**
     * Test of replaceAll method, of class com.sun.jbi.engine.iep.core.runtime.util.StringUtil.
     */
    public void testReplaceAll() {
        assertEquals("AA_BB.c.d.a_b",StringUtil.replaceAll("a.b.c.d.a_b","a.b", "AA_BB"));
        assertEquals("sAAme",StringUtil.replaceAll("same","a" ,"AA"));
        
       
    }

    
    
}
