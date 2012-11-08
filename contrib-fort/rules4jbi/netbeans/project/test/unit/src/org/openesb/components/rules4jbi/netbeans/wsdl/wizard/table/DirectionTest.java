/*
 * @(#)DirectionTest.java        $Revision: 1.2 $ $Date: 2009/01/27 21:42:44 $
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

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2009/01/27 21:42:44 $
 * 
 * @since 0.1
 */
public class DirectionTest {

    private Direction input;
    
    private Direction output;

    @Before
    public void setUp() {
        input = new Direction(Direction.INPUT_VALUE);
        output = new Direction(Direction.OUTPUT_VALUE);
    }

    @Test
    public void validity() {
        new Direction(Direction.INPUT_VALUE);
        new Direction(Direction.OUTPUT_VALUE);
        
        assertTrue(true);
    }

    @Test(expected=IllegalArgumentException.class)
    public void validity2() {
        new Direction("whatever");
    }
    
    @Test
    public void isInput() {
        assertFalse(output.isInput());
        assertTrue(input.isInput());
    }

    @Test
    public void isOutput() {
        assertFalse(input.isOutput());
        assertTrue(output.isOutput());
    }

    @Test
    public void toDescriptiveString() {
        assertEquals(Direction.INPUT_STRING, input.toDescriptiveString());
        assertEquals(Direction.OUTPUT_STRING, output.toDescriptiveString());
    }
}
