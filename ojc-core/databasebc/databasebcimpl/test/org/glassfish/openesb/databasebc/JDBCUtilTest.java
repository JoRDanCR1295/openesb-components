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
 * @(#)JDBCUtilTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc;

import junit.framework.TestCase;

import java.sql.Types;


public class JDBCUtilTest extends TestCase {
    JDBCUtil instance = null;

    //@Override
	public void setUp() throws Exception {
        instance = new JDBCUtil();
    }

    //@Override
	public void tearDown() throws Exception {
    }

    public void testConvert() throws Exception {
        final boolean b = true;
        final String value = "true";
        final Boolean bool = (Boolean) JDBCUtil.convert(value, Types.BOOLEAN);

        if (bool.booleanValue() == b) {
            System.out.println("Test passed");
        } else {
            System.out.println("Test failed");
        }
    }
}
