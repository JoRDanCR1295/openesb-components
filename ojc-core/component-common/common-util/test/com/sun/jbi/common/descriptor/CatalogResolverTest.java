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
 * @(#)CatalogResolverTest.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor;

import java.io.File;
import junit.framework.TestCase;

/**
 * Tests {@link CatalogResolver} implementations.
 * @author Kevan Simpson
 */
public class CatalogResolverTest extends TestCase {

	/**
	 * @param name
	 */
	public CatalogResolverTest(String name) {
		super(name);
	}

	public void testResolve() throws Exception {
	    String suPath = (new File(this.getClass().getResource("catalog").toURI())).getAbsolutePath();
	    CatalogResolver cr = CatalogResolver.newInstance(suPath);
	    assertNotNull("resolver is null", cr);
	    File wsdl = cr.resolveFile("Synchronous/Synchronous.wsdl");
	    assertNotNull("wsdl is null", wsdl);
	    assertTrue("wsdl does not exist", wsdl.exists());
	}
}
