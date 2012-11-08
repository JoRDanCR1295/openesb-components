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
 * @(#)HLLPDecoderTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.llp;

import junit.framework.*;
import java.io.Serializable;
import java.io.*;

import org.jmock.core.*;
import org.jmock.*;

import java.util.logging.Level;
import java.util.logging.Logger;



/**
 *
 * @author traghuna
 */
public class HLLPDecoderTest extends org.jmock.cglib.MockObjectTestCase {
    HLLPDecoder instance =new HLLPDecoder();
    
  
    
    public HLLPDecoderTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        
     }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(HLLPDecoderTest.class);
        
        return suite;
    }

  /**
     * Test of setStartBlockChar and getStartBlockChar method.
     */
	 public void testSetGetStartBlockCharacter() {
		System.out.println("Testing setStartBlockCharacter and getStartBlockCharacter");
      try{  
			char val = (char)11;
			instance.setStartBlockChar(val);
			char result = instance.getStartBlockChar();
			assertEquals(val, result);
			System.out.println("Successfully tested setStartBlockCharacter and getStartBlockCharacter");
       } catch(Exception e) {
            System.out.println("Exception occred for setStartBlockCharacter and getStartBlockCharacter");
		}
	 }

	 /**
     * Test of setEndDataChar and getEndDataChar method.
     */
	 public void testSetGetEndDataCharacter() {
		System.out.println("Testing setEndDataChar and getEndDataChar");
      try{  
			char val = (char)28;
			instance.setEndDataChar(val);
			char result = instance.getEndDataChar();
			assertEquals(val, result);
			System.out.println("Successfully tested setEndDataCharacter and getEndDataCharacter");
       } catch(Exception e) {
            System.out.println("Exception occred for setEndDataCharacter and getEndDataCharacter");
		}
	 }

	 /**
     * Test of setEndBlockChar and getEndBlockChar method.
     */
	 public void testSetGetEndBlockCharacter() {
		System.out.println("Testing setStartBlockCharacter and getStartBlockCharacter");
      try{  
			char val = (char)13;
			instance.setEndBlockChar(val);
			char result = instance.getEndBlockChar();
			assertEquals(val, result);
			System.out.println("Successfully tested setEndBlockCharacter and getEndBlockCharacter");
       } catch(Exception e) {
            System.out.println("Exception occred for setEndBlockCharacter and getEndBlockCharacter");
		}
	 }
    
	 /**
     * Test of setversionID and getversionID method.
     */
	 public void testSetGetVersionID() {
		System.out.println("Testing setVersionID and getVersionID");
      try{  
			String val = "2.1";
			instance.setVersionID(val);
			String result = instance.getVersionID();
			assertEquals(val, result);
			System.out.println("Successfully tested setVersionID and getVersionID");
       } catch(Exception e) {
            System.out.println("Exception occred for setVersionID and getVersionID");
		}
	 }

	  /**
     * Test of setBlockType and getBlockType method.
     */
	 public void testSetGetBlockType() {
		System.out.println("Testing setBlockType and getBlockType");
      try{  
			char val = 'D';
			instance.setBlockType(val);
			char result = instance.getBlockType();
			assertEquals(val, result);
			System.out.println("Successfully tested setBlockType and getBlockType");
       } catch(Exception e) {
            System.out.println("Exception occred for setBlockType and getBlockType");
		}
	 }

      /**
     * Test of setHLLPChecksumEnabled and getHLLPChecksumEnabled method.
     */
	 public void testSetGetHLLPChecksumEnabled() {
		System.out.println("Testing setHLLPChecksumEnabled and getHLLPChecksumEnabled");
      try{  
			boolean val = true;
			instance.setHLLPChecksumEnabled(val);
			boolean result = instance.getHLLPChecksumEnabled();
			assertEquals(val, result);
			System.out.println("Successfully tested setHLLPChecksumEnabled and getHLLPChecksumEnabled");
       } catch(Exception e) {
            System.out.println("Exception occred for setHLLPChecksumEnabled and getHLLPChecksumEnabled");
		}
	 }
}
