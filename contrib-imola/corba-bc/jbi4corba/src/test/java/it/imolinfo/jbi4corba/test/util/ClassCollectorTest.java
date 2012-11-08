 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.util;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import it.imolinfo.jbi4corba.webservice.generator.Util;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;

import junit.framework.TestCase;

public class ClassCollectorTest extends TestCase {

    private static final Logger LOG = LoggerFactory.getLogger(ClassCollectorTest.class);

    public ClassCollectorTest(String arg0) {
        super(arg0);
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }


    public void testRicorsive() throws Exception {
        String dir = new File(".").getAbsolutePath();
        List<File> list = new ArrayList<File>();

        list.add(new File(dir + "/it/imolinfo/jbi4corba/test/util/FakeL0.class"));

        Set<Class> res = Util.findClassUsed(dir, list);

        assertNotNull(res);
        LOG.debug("The classes collected are " + res.size());

        assertEquals(7, res.size());

        // TODO improve asserts
    }

}
