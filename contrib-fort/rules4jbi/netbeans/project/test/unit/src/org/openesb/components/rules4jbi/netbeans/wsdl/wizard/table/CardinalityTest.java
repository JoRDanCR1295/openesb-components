/*
 * @(#)CardinalityTest.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:21 $
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

package org.openesb.components.rules4jbi.netbeans.wsdl.wizard.table;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:21 $
 * 
 * @since 0.1
 */
public class CardinalityTest {

    @Test
    public void isValidCardinalityValue() {
        assertFalse(Cardinality.isValidCardinalityValue(null));
        assertFalse(Cardinality.isValidCardinalityValue(""));
        assertFalse(Cardinality.isValidCardinalityValue(" "));
        assertFalse(Cardinality.isValidCardinalityValue("     "));
        assertFalse(Cardinality.isValidCardinalityValue("     "));
        assertFalse(Cardinality.isValidCardinalityValue("0"));
        assertFalse(Cardinality.isValidCardinalityValue("-1"));
        assertFalse(Cardinality.isValidCardinalityValue("-305"));
        assertFalse(Cardinality.isValidCardinalityValue("10*"));
        assertFalse(Cardinality.isValidCardinalityValue(" *"));
        assertFalse(Cardinality.isValidCardinalityValue("* "));
        assertFalse(Cardinality.isValidCardinalityValue("aBCdef"));
        assertFalse(Cardinality.isValidCardinalityValue(Integer.toString(Integer.MIN_VALUE)));
        assertFalse(Cardinality.isValidCardinalityValue(Integer.toString(Integer.MAX_VALUE)));
        
        assertTrue(Cardinality.isValidCardinalityValue("*"));
        assertTrue(Cardinality.isValidCardinalityValue("1"));
        assertTrue(Cardinality.isValidCardinalityValue("5"));
        assertTrue(Cardinality.isValidCardinalityValue("12"));
        assertTrue(Cardinality.isValidCardinalityValue("345129"));
        assertTrue(Cardinality.isValidCardinalityValue(Integer.toString(Integer.MAX_VALUE - 1)));
    }

    @Test
    public void intValue() {
        Cardinality cardinality = new Cardinality("1");
        assertEquals(1, cardinality.intValue());
        
        cardinality = new Cardinality("12");
        assertEquals(12, cardinality.intValue());
        
        cardinality = new Cardinality("*");
        assertEquals(Integer.MAX_VALUE, cardinality.intValue());
    }

    @Test
    public void isUnbounded() {
        Cardinality cardinality = new Cardinality("1");
        assertFalse(cardinality.isUnbounded());
        
        cardinality = new Cardinality("12");
        assertFalse(cardinality.isUnbounded());
        
        cardinality = new Cardinality("*");
        assertTrue(cardinality.isUnbounded());
    }

    @Test
    public void toDescriptiveString() {
        Cardinality normalCardinality = new Cardinality("12");
        Cardinality unboundedCardinality = new Cardinality("*");
        
        assertEquals("12", normalCardinality.toDescriptiveString());
        assertEquals(Cardinality.UNBOUNDED_STRING, unboundedCardinality.toDescriptiveString());
    }
}
