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
 * @(#)BasicTypeConverterTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.util;

import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import junit.framework.TestCase;

import org.apache.commons.jxpath.NodeSet;
import org.apache.commons.jxpath.Pointer;

/**
 * Tests BasicTypeConverter
 * 
 * @author Dmitri Plotnikov
 * @version  
 */

public class BasicTypeConverterTest extends TestCase {
    /**
     * Construct a new instance of this test case.
     *
     * @param name Name of the test case
     */
    public BasicTypeConverterTest(String name) {
        super(name);
    }

    public void testPrimitiveToString() {
        assertConversion(new Integer(1), String.class, "1");
    }

    public void testArrayToList() {
        assertConversion(
            new int[] { 1, 2 },
            List.class,
            Arrays.asList(new Object[] { new Integer(1), new Integer(2)}));
    }

    public void testArrayToArray() {
        assertConversion(
            new int[] { 1, 2 },
            String[].class,
            Arrays.asList(new String[] { "1", "2" }));
    }

    public void testListToArray() {
        assertConversion(
            Arrays.asList(new Integer[] { new Integer(1), new Integer(2)}),
            String[].class,
            Arrays.asList(new String[] { "1", "2" }));

        assertConversion(
            Arrays.asList(new String[] { "1", "2" }),
            int[].class,
            Arrays.asList(new Integer[] { new Integer(1), new Integer(2)}));
    }

    public void testInvalidConversion() {
        boolean exception = false;
        try {
            TypeUtils.convert("'foo'", Date.class);
        }
        catch (Throwable ex) {
            exception = true;
        }
        assertTrue("Type conversion exception", exception);
    }

    public void assertConversion(Object from, Class toType, Object expected) {
        boolean can = TypeUtils.canConvert(from, toType);
        assertTrue("Can convert: " + from.getClass() + " to " + toType, can);
        Object result = TypeUtils.convert(from, toType);
        if (result.getClass().isArray()) {
            ArrayList list = new ArrayList();
            for (int j = 0; j < Array.getLength(result); j++) {
                list.add(Array.get(result, j));
            }
            result = list;
        }
        assertEquals(
            "Convert: " + from.getClass() + " to " + toType,
            expected,
            result);
    }
    
    public void testSingletonCollectionToString() {
        assertConversion(Collections.singleton("Earth"), String.class, "Earth");
    }

    public void testSingletonArrayToString() {
        assertConversion(new String[] { "Earth" }, String.class, "Earth");
    }

    public void testPointerToString() {
        assertConversion(new Pointer() {
            public Object getValue() {
                return "value";
            }
            public Object getNode() {
                return null;
            }
            public void setValue(Object value) {
            }
            public Object getRootNode() {
                return null;
            }
            public String asPath() {
                return null;
            }
            public Object clone() {
                return null;
            }
            public int compareTo(Object o) {
                return 0;
            }
        }, String.class, "value");
    }

    public void testNodeSetToString() {
        assertConversion(new NodeSet() {
            public List getNodes() {
                return null;
            }
            public List getPointers() {
                return null;
            }
            public List getValues() {
                List list = new ArrayList();
                list.add("hello");
                list.add("goodbye");
                return Collections.singletonList(list);
            }
        }, String.class, "hello");
    }

    // succeeds in current version
    public void testNodeSetToInteger() {
        assertConversion(new NodeSet() {
            public List getNodes() {
                return null;
            }
            public List getPointers() {
                return null;
            }
            public List getValues() {
                return Collections.singletonList("9");
            }
        }, Integer.class, new Integer(9));
    }    
    
    public void testBeanUtilsConverter() {
        assertConversion("12", BigDecimal.class, new BigDecimal(12));
    }
}
