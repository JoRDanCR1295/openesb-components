/*
 * $Id: TestDistinctRowIterator.java,v 1.1 2007/11/28 10:01:25 jawed Exp $
 * =======================================================================
 * Copyright (c) 2002-2005 Axion Development Team.  All rights reserved.
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

package org.axiondb.engine.rowiterators;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.axiondb.ColumnIdentifier;
import org.axiondb.Row;
import org.axiondb.RowIterator;
import org.axiondb.Selectable;
import org.axiondb.engine.rows.SimpleRow;

/**
 * @version $Revision: 1.1 $ $Date: 2007/11/28 10:01:25 $
 * @author Rodney Waldhoff
 * @author Ahimanikya Satapathy
 */
public class TestDistinctRowIterator extends AbstractRowIteratorTest {

    //------------------------------------------------------------ Conventional

    public TestDistinctRowIterator(String testName) {
        super(testName);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(TestDistinctRowIterator.class);
        return suite;
    }

    //---------------------------------------------------------- Abstract Impls

    public RowIterator makeRowIterator() {
        Selectable[] sel = new Selectable[] { _col};
        HashMap map = new HashMap();
        map.put(_col, new Integer(0));
        return new DistinctRowIterator(new ListIteratorRowIterator(makeFullRowList().listIterator()), map, sel);
    }
    
    protected int getSize() {
        return 2;
    }

    public List makeRowList() {
        List list = new ArrayList();
        {
            SimpleRow row = new SimpleRow(1);
            row.set(0, "xyzzy1");
            list.add(row);
        }
        {
            SimpleRow row = new SimpleRow(1);
            row.set(0, "xyzzy2");
            list.add(row);
        }
        return list;
    }

    private List makeFullRowList() {
        List list = new ArrayList();
        for (int i = 0; i < 10; i++) {
            SimpleRow row = new SimpleRow(1);
            if (i % 2 == 0) {
                row.set(0, "xyzzy1");
            } else {
                row.set(0, "xyzzy2");
            }
            list.add(row);
        }
        return list;
    }

    private ColumnIdentifier _col = new ColumnIdentifier("col");

    //------------------------------------------------------------------- Tests

    public void testAddSetRemove() throws Exception {
        RowIterator rows = makeRowIterator();
        rows.next();
        Row row = new SimpleRow(1);
        row.set(0, "AA");
        
        try {
            rows.set(row);
            fail("Expected UnsupportedOperationException");
        } catch (UnsupportedOperationException e) {

        }

        try {
            rows.remove();
            fail("Expected UnsupportedOperationException");
        } catch (UnsupportedOperationException e) {

        }
    }
}