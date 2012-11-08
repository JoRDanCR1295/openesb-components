/*
 * @(#)ValidatorTest.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.util;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @since 0.1
 */
public class ValidatorTest {

    @Test
    public void isValidNCName() {
        assertFalse(Validator.isValidNCName(null));
        assertFalse(Validator.isValidNCName("&#^%#:&*("));
        assertFalse(Validator.isValidNCName("123"));
        assertFalse(Validator.isValidNCName("aaa:bbb"));
        
        assertTrue(Validator.isValidNCName("Abcd"));
        assertTrue(Validator.isValidNCName("_qwerty-JKlp"));
        assertTrue(Validator.isValidNCName("GHJ.ti.-JKlp"));
    }

    @Test
    public void isValidFileName() {
        assertFalse(Validator.isValidFileName(null));
        assertFalse(Validator.isValidFileName(""));
        assertFalse(Validator.isValidFileName("   "));
        assertFalse(Validator.isValidFileName("   abc"));
        assertFalse(Validator.isValidFileName("abc   "));
        assertFalse(Validator.isValidFileName("asff/fdsf"));
        assertFalse(Validator.isValidFileName("asff\\fdsf"));
        assertFalse(Validator.isValidFileName("rrwe:AAAA"));
        assertFalse(Validator.isValidFileName("rrwe|AAAA"));
        assertFalse(Validator.isValidFileName("YOIon%ffdsfldsf&m"));
        
        assertTrue(Validator.isValidFileName("abc.txt"));
        assertTrue(Validator.isValidFileName("AbC.TXt"));
        assertTrue(Validator.isValidFileName("_Aa Bbb Ccc DdD"));
        assertTrue(Validator.isValidFileName("a"));
        assertTrue(Validator.isValidFileName("aB"));
        assertTrue(Validator.isValidFileName("_bC"));
    }
    
    @Test
    public void isValidURI() {
        assertFalse(Validator.isValidURI(null));
        assertFalse(Validator.isValidURI(""));
        assertFalse(Validator.isValidURI("    "));
        assertFalse(Validator.isValidURI("http://www.abc.com/something    "));
        assertFalse(Validator.isValidURI("   http://www.abc.com/something"));
        assertFalse(Validator.isValidURI("http://www dot abc dot com/something"));
        assertFalse(Validator.isValidURI("http://www.ab%c%d.com/something"));
        assertFalse(Validator.isValidURI("fksdj fj lkasd alkjds f"));
        
        assertTrue(Validator.isValidURI("http://www.abc.com/whatever"));
    }
}
