/*
 * $Id: BaseConstraintTest.java,v 1.1 2007/11/28 10:01:22 jawed Exp $
 * =======================================================================
 * Copyright (c) 2002-2003 Axion Development Team.  All rights reserved.
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

package org.axiondb.constraints;

import java.io.Serializable;

import org.axiondb.BaseSerializableTest;
import org.axiondb.Column;
import org.axiondb.Constraint;
import org.axiondb.Table;
import org.axiondb.engine.rows.SimpleRow;
import org.axiondb.engine.tables.MemoryTable;
import org.axiondb.types.CharacterVaryingType;
import org.axiondb.types.IntegerType;

/**
 * @version $Revision: 1.1 $ $Date: 2007/11/28 10:01:22 $
 * @author Rodney Waldhoff
 */
public abstract class BaseConstraintTest extends BaseSerializableTest {

    //------------------------------------------------------------ Conventional

    public BaseConstraintTest(String testName) {
        super(testName);        
    }
    
    //--------------------------------------------------------------- Lifecycle
    
    public void setUp() throws Exception {
        super.setUp();
        Table t = new MemoryTable("FOO");
        t.addColumn(new Column("NAME",new CharacterVaryingType(10)));
        t.addColumn(new Column("NUM",new IntegerType()));
        setTable(t);
    }

    public void tearDown() throws Exception {
        super.tearDown();
        setTable(null);
    }

    //---------------------------------------------------------------- Framework

    protected abstract Constraint createConstraint();
    protected abstract Constraint createConstraint(String name);

    protected Serializable makeSerializable() {
        return createConstraint();
    }

    private Table _table = null;

    protected Table getTable() {
        return _table;
    }

    private void setTable(Table t) {
        _table = t;
    }

    //-------------------------------------------------------------------- Util

    protected SimpleRow createRow(String name, Integer num) {
        SimpleRow row = new SimpleRow(2);
        row.set(0,name);
        row.set(1,num);
        return row;
    }

    //------------------------------------------------------------------- Tests

    public void testGeneratedName() throws Exception {
        Constraint constraint = createConstraint();
        assertNotNull(constraint.getName());
        assertEquals(constraint.getName().toUpperCase(),constraint.getName());
        Constraint constraint2 = createConstraint();
        assertNotNull(constraint2.getName());
        assertEquals(constraint2.getName().toUpperCase(),constraint2.getName());
        assertTrue(!constraint.getName().equals(constraint2.getName()));
    }

    public void testNameIsUpperCase() throws Exception {
        Constraint constraint = createConstraint("FOO");
        assertEquals("FOO",constraint.getName());
    }

    public void testGetType() throws Exception {
        Constraint constraint = createConstraint("FOO");
        assertNotNull(constraint.getType());
    }
}

