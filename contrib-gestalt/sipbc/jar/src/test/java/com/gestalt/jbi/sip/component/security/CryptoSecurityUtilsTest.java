package com.gestalt.jbi.sip.component.security;

import junit.framework.TestCase;


public class CryptoSecurityUtilsTest extends TestCase {
    public void testEncoding() throws Exception {
        CryptoSecurityUtils sec = new CryptoSecurityUtils();

        String data = "p4ssw0rd";
        String endata = sec.encrypt(data);
        assertTrue("Data was not encrypted", !data.equals(endata));
        endata = sec.decrypt(endata);
        assertEquals("Data was not same after decrypt", endata, data);

        sec.cleanup();
    }
}
