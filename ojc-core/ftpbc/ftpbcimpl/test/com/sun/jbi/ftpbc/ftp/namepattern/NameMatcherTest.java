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
 * @(#)NameMatcherTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ftpbc.ftp.namepattern;

import java.security.SecureRandom;
import java.text.DateFormatSymbols;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
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
public class NameMatcherTest extends TestCase {
    private NameMatcher instance;
    
    private DateFormatSymbols symbols;
    private String pattern;
    private NamePattern np;
    private NameMatcher nm;
    private String wellFormed;
    private String malFormed;
    private boolean matches;
    private boolean find;
    private boolean lookingAt;
    
    public NameMatcherTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        super.setUp();
        instance = new NameMatcher("Nothing to be matched");
        symbols = new DateFormatSymbols(DateFormatLocale.LOCALE_IN_USE);
    }

    protected void tearDown() throws Exception {
        instance = null;
        super.tearDown();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(NameMatcherTest.class);
        
        return suite;
    }

    /**
     * Test of getWorkingFileName method, of class com.sun.jbi.ftpbc.NameMatcher.
     */
    public void testGetWorkingFileName() throws Exception {
        System.out.println("getWorkingFileName");
        
        String expResult = null;
        String result = instance.getWorkingFileName();
        assertEquals(expResult, result);
        
    }

    /**
     * Test of getRawPattern method, of class com.sun.jbi.ftpbc.NameMatcher.
     */
    public void testGetRawPattern() throws Exception {
        System.out.println("getRawPattern");
        
        String expResult = "Nothing to be matched";
        String result = instance.getRawPattern();
        assertEquals(expResult, result);
        
    }

    /**
     * Test of getRegexPattern method, of class com.sun.jbi.ftpbc.NameMatcher.
     */
    public void testGetRegexPattern() throws Exception {
        System.out.println("getRegexPattern");
        
        String expResult = "Nothing to be matched";
        Pattern result = instance.getRegexPattern();
        assertNotNull(result);
        assertEquals(expResult, result.pattern());
        
    }

    /**
     * Test of getRegexMatcher method, of class com.sun.jbi.ftpbc.NameMatcher.
     */
    public void testGetRegexMatcher() throws Exception {
        System.out.println("getRegexMatcher");
        
        String input = "file2006.Monday";
        Matcher result = instance.getRegexMatcher(input);
        assertNotNull(result);
        assertEquals(false, result.matches());
        
    }

    /**
     * Test of matches method, of class com.sun.jbi.ftpbc.NameMatcher.
     */
    public void testMatches() throws Exception {
        System.out.println("matches");
        
        String input = "no expected matching";
        
        boolean expResult = false;
        boolean result = instance.matches(input);
        assertEquals(expResult, result);
        
        try {
            instance.matches(null);
            fail("An exception is expected: null input");
        } catch (Exception e) {
            e.printStackTrace();
        }
        
    }

    /**
     * Test of find method, of class com.sun.jbi.ftpbc.NameMatcher.
     */
    public void testFind() throws Exception {
        System.out.println("find");
        
        String input = "no expected finding";
        
        boolean expResult = false;
        boolean result = instance.find(input);
        assertEquals(expResult, result);
        
        try {
            instance.find(null);
            fail("An exception is expected: null input");
        } catch (Exception e) {
            e.printStackTrace();
        }
        
    }

    /**
     * Test of lookingAt method, of class com.sun.jbi.ftpbc.NameMatcher.
     */
    public void testLookingAt() throws Exception {
        System.out.println("lookingAt");
        
        String input = "no expected looking";
        
        boolean expResult = false;
        boolean result = instance.lookingAt(input);
        assertEquals(expResult, result);
        
        try {
            instance.lookingAt(null);
            fail("An exception is expected: null input");
        } catch (Exception e) {
            e.printStackTrace();
        }
        
    }

    
    public void testEchoSpecialRegexChars() throws Exception {
        System.out.println("testEchoSpecialRegexChars");

        // Special regex chars need to be escaped - the random test also can generate these chars
        String specialCharsTemplate = "*[]()|+{}:.^$?\\";
        pattern = specialCharsTemplate;
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        wellFormed = np.expand();
        assertEquals("nothing is expanded", pattern, wellFormed);
        matches = nm.matches(wellFormed);
        assertEquals(pattern + "==>" + wellFormed, true, matches);
        System.out.println("Special regex chars need to be escaped. " + pattern + "==>" + wellFormed + " ==> matches? " + matches);
        assertFalse("Regex pattern is escaped already", pattern.equals(nm.getRegexPattern()));
        System.out.println("Regex pattern is escaped already ==> " + nm.getRegexPattern());
        
        // combined with other chars
        pattern = "abc";
        for (int i = 0; i < specialCharsTemplate.length(); i++) {
            pattern = pattern + specialCharsTemplate.charAt(i) + i;
            np = new NamePattern(pattern);
            nm = new NameMatcher(pattern);
            wellFormed = np.expand();
            matches = nm.matches(wellFormed);
            assertEquals(pattern + "==>" + wellFormed, true, matches);
            System.out.println(i + ": " + pattern + "==>" + wellFormed + " ==> matches? " + matches);
        }
        
    }
    
    public void testPattern_u() throws Exception {
        System.out.println("testPattern_u");
        
        pattern = "%u";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        for (int i = 0; i < 10; i++) {
            wellFormed = np.expand();
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));

            // a.
            malFormed = wellFormed.substring(2);

            assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
            //? System.out.println(i + ".a. For pattern [" + pattern + "], a wellformed name [" + wellFormed + "] can be matched back, but a malformed name [" + malFormed + "] cannot be matched because of length constraints.");

            // b.
            malFormed = wellFormed.replaceFirst("-", "_");
            assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
            //? System.out.println(i + ".b. For pattern [" + pattern + "], a wellformed name [" + wellFormed + "] can be matched back, but a malformed name [" + malFormed + "] cannot be matched because of format constraints '-'.");

            // c.
            malFormed = wellFormed.replaceFirst("[abcdef]", "A");
            assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
            //? System.out.println(i + ".b. For pattern [" + pattern + "], a wellformed name [" + wellFormed + "] can be matched back, but a malformed name [" + malFormed + "] cannot be matched because of upper/lower case constraints 'A'.");

            // d.
            malFormed = wellFormed.replaceFirst("[abcdef]", "g");
            assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
            //? System.out.println(i + ".d. For pattern [" + pattern + "], a wellformed name [" + wellFormed + "] can be matched back, but a malformed name [" + malFormed + "] cannot be matched because of letter range constraints 'g'.");
        }
        
    }
    
    public void testPattern_I() throws Exception {
        System.out.println("testPattern_I");

        pattern = "%I";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        String input = "anything";
        assertEquals(pattern + "==>" + input, true, nm.matches(input));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "], an input [" + input + "] can be matched.");

        input = "";
        assertEquals(pattern + "==>" + input, true, nm.matches(input));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "], an input [" + input + "] can be matched.");
        
    }
    
    public void testPattern_f() throws Exception {
        System.out.println("testPattern_f");

        pattern = "%f";
        
        String workingFile = "abc.123";
        np = new NamePattern(pattern);
        np.setFileNameFromEgate(workingFile);
        nm = new NameMatcher(pattern, workingFile);
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");
        
        workingFile = "with.embedded.pattern.%u.and.%y%y%y%y%M%d";
        np = new NamePattern(pattern);
        np.setFileNameFromEgate(workingFile);
        nm = new NameMatcher(pattern, workingFile);
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        malFormed = "different.with.workingFile";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_G() throws Exception {
        System.out.println("testPattern_G");

        pattern = "%G";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < symbols.getEras().length; i++) {
            wellFormed = symbols.getEras()[i];
            if (null == wellFormed || wellFormed.length() == 0) {
                continue;
            }
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        

        malFormed = "ad";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_GGGGGG() throws Exception {
        System.out.println("testPattern_GGGGGG");

        pattern = "%G%G%G%G%G%G";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < symbols.getEras().length; i++) {
            wellFormed = symbols.getEras()[i];
            if (null == wellFormed || wellFormed.length() == 0) {
                continue;
            }
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        

        malFormed = "bc";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_y() throws Exception {
        System.out.println("testPattern_y");

        pattern = "%y";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 100; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "006";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "2006";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "002006";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_yy() throws Exception {
        System.out.println("testPattern_yy");

        pattern = "%y%y";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 100; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "006";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "2006";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "002006";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_yyy() throws Exception {
        System.out.println("testPattern_yyy");

        pattern = "%y%y%y";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 100; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "006";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "2006";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "002006";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_yyyy() throws Exception {
        System.out.println("testPattern_yyyy");

        pattern = "%y%y%y%y";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 100; i++) {
            wellFormed = "00" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 100; i < 1000; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 1990; i < 2050; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "06";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "006";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "002006";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_yyyyyy() throws Exception {
        System.out.println("testPattern_yyyyyy");

        pattern = "%y%y%y%y%y%y";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1990; i < 2050; i++) {
            wellFormed = "00" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "06";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "006";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "2006";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "02006";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0002006";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_M() throws Exception {
        System.out.println("testPattern_M");

        pattern = "%M";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 13; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "13";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "012";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_MM() throws Exception {
        System.out.println("testPattern_MM");

        pattern = "%M%M";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 10; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 13; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "13";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_MMM() throws Exception {
        System.out.println("testPattern_MMM");

        pattern = "%M%M%M";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < symbols.getShortMonths().length; i++) {
            wellFormed = symbols.getShortMonths()[i];
            if (null == wellFormed || wellFormed.length() == 0) {
                continue;
            }
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }

        malFormed = "nov";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "DEC";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_MMMM() throws Exception {
        System.out.println("testPattern_MMMM");

        pattern = "%M%M%M%M";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < symbols.getMonths().length; i++) {
            wellFormed = symbols.getMonths()[i];
            if (null == wellFormed || wellFormed.length() == 0) {
                continue;
            }
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "october";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "NOVEMBER";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_MMMMMM() throws Exception {
        System.out.println("testPattern_MMMMMM");

        pattern = "%M%M%M%M%M%M";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < symbols.getMonths().length; i++) {
            wellFormed = symbols.getMonths()[i];
            if (null == wellFormed || wellFormed.length() == 0) {
                continue;
            }
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }

        malFormed = "october";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "NOVEMBER";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_w() throws Exception {
        System.out.println("testPattern_w");

        pattern = "%w";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 55; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "55";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_ww() throws Exception {
        System.out.println("testPattern_ww");

        pattern = "%w%w";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 10; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 55; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "010";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "55";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_wwwwww() throws Exception {
        System.out.println("testPattern_wwwwww");

        pattern = "%w%w%w%w%w%w";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 10; i++) {
            wellFormed = "00000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 55; i++) {
            wellFormed = "0000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "55";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000055";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_W() throws Exception {
        System.out.println("testPattern_W");

        pattern = "%W";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 6; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");

        // since 6 is a valid week number in month
        // should use 7 instead here
//        malFormed = "6";
        malFormed = "7";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_WWWWWW() throws Exception {
        System.out.println("testPattern_WWWWWW");

        pattern = "%W%W%W%W%W%W";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 7; i++) {
            wellFormed = "00000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        // since 6 is a valid number for weeks in month - 6 is a valid value
        // in order to keep the flavor of the test - use 7 instead
//        malFormed = "6";
        malFormed = "7";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        // since 6 is a valid number for weeks in month - 000006 is a valid value
        // in order to keep the flavor of the test - use 000007 instead
//        malFormed = "000006";
        malFormed = "000007";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_D() throws Exception {
        System.out.println("testPattern_D");

        pattern = "%D";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 367; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "367";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_DD() throws Exception {
        System.out.println("testPattern_DD");

        pattern = "%D%D";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 10; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 367; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "367";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_DDD() throws Exception {
        System.out.println("testPattern_DDD");

        pattern = "%D%D%D";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 10; i++) {
            wellFormed = "00" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 100; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 100; i < 367; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "367";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }

    public void testPattern_DDDDDD() throws Exception {
        System.out.println("testPattern_DDDDDD");

        pattern = "%D%D%D%D%D%D";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 10; i++) {
            wellFormed = "00000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 100; i++) {
            wellFormed = "0000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 100; i < 367; i++) {
            wellFormed = "000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "367";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0000001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }

    public void testPattern_d() throws Exception {
        System.out.println("testPattern_d");

        pattern = "%d";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 32; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "32";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "012";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_dd() throws Exception {
        System.out.println("testPattern_dd");

        pattern = "%d%d";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 10; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 32; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "32";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_dddddd() throws Exception {
        System.out.println("testPattern_dddddd");

        pattern = "%d%d%d%d%d%d";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 10; i++) {
            wellFormed = "00000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 32; i++) {
            wellFormed = "0000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "32";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000032";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_F() throws Exception {
        System.out.println("testPattern_F");

        pattern = "%F";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 6; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "6";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_FFFFFF() throws Exception {
        System.out.println("testPattern_FFFFFF");

        pattern = "%F%F%F%F%F%F";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 6; i++) {
            wellFormed = "00000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "6";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000006";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_E() throws Exception {
        System.out.println("testPattern_E");

        pattern = "%E";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < symbols.getShortWeekdays().length; i++) {
            wellFormed = symbols.getShortWeekdays()[i];
            if (null == wellFormed || wellFormed.length() == 0) {
                continue;
            }
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }

        malFormed = "mon";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "FRI";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_EE() throws Exception {
        System.out.println("testPattern_EE");

        pattern = "%E%E";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < symbols.getShortWeekdays().length; i++) {
            wellFormed = symbols.getShortWeekdays()[i];
            if (null == wellFormed || wellFormed.length() == 0) {
                continue;
            }
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }

        malFormed = "mon";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "FRI";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_EEE() throws Exception {
        System.out.println("testPattern_EEE");

        pattern = "%E%E%E";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < symbols.getShortWeekdays().length; i++) {
            wellFormed = symbols.getShortWeekdays()[i];
            if (null == wellFormed || wellFormed.length() == 0) {
                continue;
            }
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }

        malFormed = "mon";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "FRI";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_EEEE() throws Exception {
        System.out.println("testPattern_EEEE");

        pattern = "%E%E%E%E";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < symbols.getWeekdays().length; i++) {
            wellFormed = symbols.getWeekdays()[i];
            if (null == wellFormed || wellFormed.length() == 0) {
                continue;
            }
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "monday";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "FRIDAY";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_EEEEEE() throws Exception {
        System.out.println("testPattern_EEEEEE");

        pattern = "%E%E%E%E%E%E";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < symbols.getWeekdays().length; i++) {
            wellFormed = symbols.getWeekdays()[i];
            if (null == wellFormed || wellFormed.length() == 0) {
                continue;
            }
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "monday";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "FRIDAY";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_a() throws Exception {
        System.out.println("testPattern_a");

        pattern = "%a";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < symbols.getAmPmStrings().length; i++) {
            wellFormed = symbols.getAmPmStrings()[i];
            if (null == wellFormed || wellFormed.length() == 0) {
                continue;
            }
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "am";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "Pm";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_aaaaaa() throws Exception {
        System.out.println("testPattern_aaaaaa");

        pattern = "%a%a%a%a%a%a";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < symbols.getAmPmStrings().length; i++) {
            wellFormed = symbols.getAmPmStrings()[i];
            if (null == wellFormed || wellFormed.length() == 0) {
                continue;
            }
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "am";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "pM";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_H() throws Exception {
        System.out.println("testPattern_H");

        pattern = "%H";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 24; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "-1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "24";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_HH() throws Exception {
        System.out.println("testPattern_HH");

        pattern = "%H%H";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 24; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "-1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "24";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_HHHHHH() throws Exception {
        System.out.println("testPattern_HHHHHH");

        pattern = "%H%H%H%H%H%H";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "00000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 24; i++) {
            wellFormed = "0000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "24";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000024";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_k() throws Exception {
        System.out.println("testPattern_k");

        pattern = "%k";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 25; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "25";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_kk() throws Exception {
        System.out.println("testPattern_kk");

        pattern = "%k%k";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 10; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 25; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "25";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_kkkkkk() throws Exception {
        System.out.println("testPattern_kkkkkk");

        pattern = "%k%k%k%k%k%k";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 10; i++) {
            wellFormed = "00000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 25; i++) {
            wellFormed = "0000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "25";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000025";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_K() throws Exception {
        System.out.println("testPattern_K");

        pattern = "%K";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 12; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "-1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "12";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_KK() throws Exception {
        System.out.println("testPattern_KK");

        pattern = "%K%K";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 12; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "-1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "12";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_KKKKKK() throws Exception {
        System.out.println("testPattern_KKKKKK");

        pattern = "%K%K%K%K%K%K";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "00000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 12; i++) {
            wellFormed = "0000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "12";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000012";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_h() throws Exception {
        System.out.println("testPattern_h");

        pattern = "%h";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 13; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "13";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_hh() throws Exception {
        System.out.println("testPattern_hh");

        pattern = "%h%h";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 10; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 13; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "13";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_hhhhhh() throws Exception {
        System.out.println("testPattern_hhhhhh");

        pattern = "%h%h%h%h%h%h";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 1; i < 10; i++) {
            wellFormed = "00000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 13; i++) {
            wellFormed = "0000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "13";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000013";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_m() throws Exception {
        System.out.println("testPattern_m");

        pattern = "%m";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 60; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "-1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "60";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_mm() throws Exception {
        System.out.println("testPattern_mm");

        pattern = "%m%m";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 60; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "-1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "60";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_mmmmmm() throws Exception {
        System.out.println("testPattern_mmmmmm");

        pattern = "%m%m%m%m%m%m";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "00000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 60; i++) {
            wellFormed = "0000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "60";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000060";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_s() throws Exception {
        System.out.println("testPattern_s");

        pattern = "%s";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 60; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "-1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "60";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_ss() throws Exception {
        System.out.println("testPattern_ss");

        pattern = "%s%s";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 60; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "-1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "60";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_ssssss() throws Exception {
        System.out.println("testPattern_ssssss");

        pattern = "%s%s%s%s%s%s";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "00000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 60; i++) {
            wellFormed = "0000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "60";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000060";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_S() throws Exception {
        System.out.println("testPattern_S");

        pattern = "%S";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 1000; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "-1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_SS() throws Exception {
        System.out.println("testPattern_SS");

        pattern = "%S%S";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 1000; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "-1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_SSS() throws Exception {
        System.out.println("testPattern_SSS");

        pattern = "%S%S%S";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "00" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 100; i++) {
            wellFormed = "0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 100; i < 1000; i++) {
            wellFormed = "" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "-1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }

    public void testPattern_SSSSSS() throws Exception {
        System.out.println("testPattern_SSSSSS");

        pattern = "%S%S%S%S%S%S";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "00000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 100; i++) {
            wellFormed = "0000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 100; i < 1000; i++) {
            wellFormed = "000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "-1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "001000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0000000";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "01";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "00001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "0000001";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }

    public void testPattern_z() throws Exception {
        System.out.println("testPattern_z");

        pattern = "%z";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        wellFormed = "abc";
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("For pattern [" + pattern + "], a wellFormed name [" + wellFormed + "] can be matched.");
        
        wellFormed = "abcd";
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("For pattern [" + pattern + "], a wellFormed name [" + wellFormed + "] can be matched.");
        
        ////////////////////////////////////////////////
        malFormed = "a";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "ab";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "abcde";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_zz() throws Exception {
        System.out.println("testPattern_zz");

        pattern = "%z%z";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        wellFormed = "abc";
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("For pattern [" + pattern + "], a wellFormed name [" + wellFormed + "] can be matched.");
        
        wellFormed = "abcd";
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("For pattern [" + pattern + "], a wellFormed name [" + wellFormed + "] can be matched.");
        
        ////////////////////////////////////////////////
        malFormed = "a";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "ab";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "abcde";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_zzz() throws Exception {
        System.out.println("testPattern_zzz");

        pattern = "%z%z%z";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        wellFormed = "abc";
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("For pattern [" + pattern + "], a wellFormed name [" + wellFormed + "] can be matched.");
        
        wellFormed = "abcd";
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("For pattern [" + pattern + "], a wellFormed name [" + wellFormed + "] can be matched.");
        
        ////////////////////////////////////////////////
        malFormed = "a";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "ab";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "abcde";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_zzzz() throws Exception {
        System.out.println("testPattern_zzzz");

        pattern = "%z%z%z%z";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        wellFormed = "abc";
        for (int i = 1; i < 100; i++) {
            wellFormed = wellFormed + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "a";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "ab";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "abc";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_zzzzzz() throws Exception {
        System.out.println("testPattern_zzzzzz");

        pattern = "%z%z%z%z%z%z";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        wellFormed = "abc";
        for (int i = 1; i < 100; i++) {
            wellFormed = wellFormed + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "a";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "ab";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "abc";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_Z() throws Exception {
        System.out.println("testPattern_Z");

        pattern = "%Z";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "-000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 100; i++) {
            wellFormed = "-00" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 100; i < 1000; i++) {
            wellFormed = "-0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 1000; i < 10000; i++) {
            wellFormed = "-" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "-1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "-12";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "-123";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "-12345";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "-123456";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testPattern_ZZZZZZ() throws Exception {
        System.out.println("testPattern_ZZZZZZ");

        pattern = "%Z%Z%Z%Z%Z%Z";
        
        np = new NamePattern(pattern);
        nm = new NameMatcher(pattern);
        
        wellFormed = np.expand();
        assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
        //? System.out.println("Pattern [" + pattern + "] is resolved into [" + wellFormed + "] and can be matched back properly.");

        for (int i = 0; i < 10; i++) {
            wellFormed = "-000" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 10; i < 100; i++) {
            wellFormed = "-00" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 100; i < 1000; i++) {
            wellFormed = "-0" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        for (int i = 1000; i < 10000; i++) {
            wellFormed = "-" + i;
            assertEquals(pattern + "==>" + wellFormed, true, nm.matches(wellFormed));
            //? System.out.println(i + ". For pattern [" + pattern + "], a name [" + wellFormed + "] can be matched.");
        }
        
        malFormed = "-1";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "-12";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "-123";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "-12345";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
        malFormed = "-123456";
        assertEquals(pattern + "==>" + malFormed, false, nm.matches(malFormed));
        //? System.out.println("For pattern [" + pattern + "], a malformed name [" + malFormed + "] cannot be matched.");
        
    }
    
    public void testRegex () throws Exception {
        System.out.println ("testRegex");
        
        pattern = "Containing regex control chars *[]()|+{}:.^$?\\, it will fail to compile using plain java.util.regex but will be ok with NameMatcher.";
        String input = pattern;
        try {
            Pattern.compile(pattern);
            fail("An exception is expected: plain java.util.regex doesn't allow it.");
        } catch (Exception e) {
            e.printStackTrace();
        }
        
        nm = new NameMatcher(pattern);
        matches = nm.matches(input);
        assertEquals(true, matches);
        System.out.println(pattern + "==>[" + nm.getRegexPattern().pattern() + "] matches? " + matches);
        
    }
    
    /**
     * Test of constructor, of class com.sun.jbi.ftpbc.NameMatcher.
     */
    public void testNameMatcher () throws Exception {
        System.out.println ("NameMatcher");
        // (%([IufGyMdhHmsSEDFwWakKzZ0-9]))*
        assertNotNull (new NameMatcher ("allowed special pattern"));
        assertNotNull (new NameMatcher ("allowed special pattern", 0));
        assertNotNull (new NameMatcher ("allowed special pattern", "working file name"));
        assertNotNull (new NameMatcher ("allowed special pattern", "working file name", 0));

        // null
        assertNotNull (new NameMatcher (null));
        assertNotNull (new NameMatcher (null, 0));
        assertNotNull (new NameMatcher (null, null));
        assertNotNull (new NameMatcher (null, null, 0));
    }
    
    public static void main(java.lang.String[] argList) {
        junit.textui.TestRunner.run(suite());
    }
    
}
