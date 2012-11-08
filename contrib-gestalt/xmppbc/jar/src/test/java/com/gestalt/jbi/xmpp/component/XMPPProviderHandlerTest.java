package com.gestalt.jbi.xmpp.component;

import junit.framework.TestCase;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import java.net.URL;

import java.util.List;


/**
 * Created by IntelliJ IDEA.
 * User: tchase
 * Date: Sep 13, 2007
 * Time: 2:25:43 PM
 * To change this template use File | Settings | File Templates.
 */
public class XMPPProviderHandlerTest extends TestCase {
    private static final String JBI_RESOURCE_NO_NAMESPACE = "/jbipart.xml";
    private static final String JBI_RESOURCE_WITH_NAMESPACE = "/jbipartsWithNameSpace.xml";

    public void testAddPartsXMLNoNameSpace() throws IOException {
        File jbiFile = getFile(JBI_RESOURCE_NO_NAMESPACE);
        XMPPProviderHandler xmppProviderHandler = new XMPPProviderHandler(null);
        BufferedReader reader = new BufferedReader(new FileReader(jbiFile));

        StringBuilder buffer = new StringBuilder();
        String temp = null;

        while ((temp = reader.readLine()) != null)
            buffer.append(temp);

        System.out.println("buffer = " + buffer.toString());

        List<String> list = xmppProviderHandler.addPartsXML(buffer.toString());
        assertTrue(list.size() > 0);

        for (String part : list) {
            System.out.println("Part is: " + part);
        }
    }

    public void testAddPartsXMLWithNameSpace() throws IOException {
        File jbiFile = getFile(JBI_RESOURCE_WITH_NAMESPACE);
        XMPPProviderHandler xmppProviderHandler = new XMPPProviderHandler(null);
        BufferedReader reader = new BufferedReader(new FileReader(jbiFile));

        StringBuilder buffer = new StringBuilder();
        String temp = null;

        while ((temp = reader.readLine()) != null)
            buffer.append(temp);

        System.out.println("buffer = " + buffer.toString());

        List<String> list = xmppProviderHandler.addPartsXML(buffer.toString());
        assertTrue(list.size() > 0);

        for (String part : list) {
            System.out.println("Part is: " + part);
        }
    }

    private File getFile(String fileName) {
        File jbiFile;
        URL url = this.getClass().getResource(fileName);
        assertNotNull(url);
        jbiFile = new File(url.getFile());
        assertNotNull(jbiFile);

        return jbiFile;
    }
}
