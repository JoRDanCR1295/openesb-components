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
 * @(#)Base64Test.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.security;

import junit.framework.TestCase;

public class Base64Test extends TestCase {
    
    /** Creates a new instance of Base64Test */
    public Base64Test() {
    }
    
    public void testEncode() {
        try {
            Base64 base64 = Base64Impl.getInstance();
            String credential = "Administrator:STC";
            String actual = base64.encode(credential).trim();
            String expected = "QWRtaW5pc3RyYXRvcjpTVEM=";

            assertEquals("Incorrect encoded results received", expected, actual);
            
        } catch (Exception e) {
            fail("Received unexpected exception: " + e.getMessage());
        }
    }
    
    public void testDecode() {
         try {
            Base64 base64 = Base64Impl.getInstance();
            String encoded = "QWRtaW5pc3RyYXRvcjpTVEM=";
            String expected = "Administrator:STC";
            String actual = base64.decode(encoded);

            assertEquals("Incorrect encoded results received", expected, actual);
            
        } catch (Exception e) {
            fail("Received unexpected exception: " + e.getMessage());
        }       
    }
}
