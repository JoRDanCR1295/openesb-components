/*
 * $Id: TestUpperFunction.java,v 1.1 2007/11/28 10:01:34 jawed Exp $
 * =======================================================================
 * Copyright (c) 2003 Axion Development Team.  All rights reserved.
 *  
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:
 * 
 * 1. Redistributions of source code must retain the above 
 *    copyright notice, this list of conditions and the following 
 *    disclaimer. 
 *   
 * 2. Redistributions in binary form must reproduce the above copyright 
 *    notice, this list of conditions and the following disclaimer in 
 *    the documentation and/or other materials provided with the 
 *    distribution. 
 *   
 * 3. The names "Tigris", "Axion", nor the names of its contributors may 
 *    not be used to endorse or promote products derived from this 
 *    software without specific prior written permission. 
 *  
 * 4. Products derived from this software may not be called "Axion", nor 
 *    may "Tigris" or "Axion" appear in their names without specific prior
 *    written permission.
 *   
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * =======================================================================
 */

package org.axiondb.functions;

import java.util.HashMap;
import java.util.Map;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.axiondb.ColumnIdentifier;
import org.axiondb.RowDecorator;
import org.axiondb.engine.rows.SimpleRow;

/**
 * @version $Revision: 1.1 $ $Date: 2007/11/28 10:01:34 $
 * @author Rodney Waldhoff
 */
public class TestUpperFunction extends BaseFunctionTest {

    //------------------------------------------------------------ Conventional

    public TestUpperFunction(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(TestUpperFunction.class);
        return suite;
    }

    //--------------------------------------------------------------- Lifecycle

    //--------------------------------------------------------------- Framework
    
    protected ConcreteFunction makeFunction() {
        return new UpperFunction();
    }
    
    //------------------------------------------------------------------- Tests

    public void testMakeNewInstance() {
        UpperFunction function = new UpperFunction();
        assertTrue(function.makeNewInstance() instanceof UpperFunction);
        assertTrue(function.makeNewInstance() != function.makeNewInstance());
    }

    public void testEvaluate() throws Exception {
        UpperFunction function = new UpperFunction();
        ColumnIdentifier sel = new ColumnIdentifier("foo");
        function.addArgument(sel);
        Map map = new HashMap();
        map.put(sel,new Integer(0));                
        RowDecorator dec = new RowDecorator(map);
        // all caps
        {
            dec.setRow(new SimpleRow(new Object[] { "FOO BAR 7" }));        
            assertEquals("FOO BAR 7",function.evaluate(dec));
        }
        // no caps
        {
            dec.setRow(new SimpleRow(new Object[] { "foo bar 7" }));        
            assertEquals("FOO BAR 7",function.evaluate(dec));
        }
        // mixed caps
        {
            dec.setRow(new SimpleRow(new Object[] { "fOo BaR 7" }));        
            assertEquals("FOO BAR 7",function.evaluate(dec));
        }
        // null
        {
            dec.setRow(new SimpleRow(new Object[] { null }));        
            assertNull(function.evaluate(dec));
        }
    }

    public void testValid() throws Exception {
        UpperFunction function = new UpperFunction();
        ColumnIdentifier sel = new ColumnIdentifier("foo");
        function.addArgument(sel);
        assertTrue(function.isValid());
    }

    public void testInvalid() throws Exception {
        UpperFunction function = new UpperFunction();
        assertTrue(! function.isValid());
        function.addArgument(new ColumnIdentifier("foo"));
        function.addArgument(new ColumnIdentifier("bar"));
        assertTrue(! function.isValid());
    }
}
