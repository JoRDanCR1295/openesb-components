/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.test;

import it.imolinfo.jbi4cics.commareaparser.CommareaLexer;
import it.imolinfo.jbi4cics.commareaparser.CommareaParser;
import it.imolinfo.jbi4cics.connection.jca.cics.CICSInteractionDescription;
import it.imolinfo.jbi4cics.locator.ServiceLocation;
import it.imolinfo.jbi4cics.locator.SimpleLocation;
import it.imolinfo.jbi4cics.messageformat.commarea.CommareaBeanMappingDescriptor;
import it.imolinfo.jbi4cics.security.J2CAccount;
import it.imolinfo.jbi4cics.webservices.descriptor.ServiceDescriptor;
import it.imolinfo.jbi4cics.webservices.runtime.ServiceCreator;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import javax.wsdl.Definition;
import javax.wsdl.factory.WSDLFactory;
import junit.framework.TestCase;

public class CreateWSDLTest extends TestCase {

    /**
     * The platform dependent line separator string.
     */
    private static final String LINE_SEPARATOR
            = System.getProperty("line.separator");

    /**
     * Creates a new instance of this class.
     */
    public CreateWSDLTest() {
    }

    public void testCreateWSDL() throws Exception {
        String copyCobol = readFileAsString(
                new File("src/test/etc/commareas/ProvaCommarea2.txt"));
        ServiceDescriptor desc = new ServiceDescriptor();
        J2CAccount account = new J2CAccount();
        SimpleLocation location = new SimpleLocation();
        CICSInteractionDescription intDesc = new CICSInteractionDescription();
        CommareaParser parser;
        CommareaBeanMappingDescriptor cbmd;
        Definition def;
        File wsdl = null;

        desc.setServiceName("SERVICE_NAME");
        desc.setServiceNameSpace("SERVICE_NAME_SPACE");
        desc.setOperationName("OPERATION_NAME");
        desc.setServiceInterfacePackageName("SERVICE.PACKAGE.NAME");
        desc.setServiceInterfaceName("INTERFACE_NAME");
        desc.setInputBeanClassName("INPUT_BEAN_NAME");
        desc.setOutputBeanClassName("OUTPUT_BEAN_NAME");
        account.setUsername("USERNAME");
        account.setPassword("PASSWORD");
        desc.setAccount(account);
        location.setConnectionType(ServiceLocation.CICS);
        location.setLocationName("JNDI_CONNECTION_NAME");
        desc.setServiceLocation(location);
        intDesc.setProgramName("PROGRAM_NAME");
        intDesc.setTransactionName("TRANSACTION_NAME");
        intDesc.setTpn(true);
        desc.setInteractionDescription(intDesc);
        parser = new CommareaParser(
                new CommareaLexer(new StringReader(copyCobol)));
        cbmd = parser.commarea_definition();
        desc.setInputMappingDescriptor(cbmd);
        desc.setOutputMappingDescriptor(cbmd);
        desc.setCodePage("CP037");

        def  = new ServiceCreator().createWsdlFromCopyCobol(copyCobol, null,
                                                            desc);
        wsdl = File.createTempFile("ProvaCommarea2_actual", null);
        try {
            int rowCount;

            WSDLFactory.newInstance().newWSDLWriter().writeWSDL(
                    def, new FileWriter(wsdl));
            rowCount = compareFiles(new File(
                    "src/test/etc/wsdl/ProvaCommarea2_expected.wsdl"), wsdl);
            assertEquals("Wrong lines number in the created WSDL file", 106,
                         rowCount);
        } finally {
            wsdl.delete();
        }
    }

    private static int compareFiles(
            final File expected, final File actual) throws IOException {
        BufferedReader expectedR = new BufferedReader(new FileReader(expected));
        BufferedReader actualR = new BufferedReader(new FileReader(actual));
        int rowCompared = 0;
        String exp = expectedR.readLine();
        String act = actualR.readLine();

        while ((exp != null) && (act != null)) {
            exp = exp.trim();
            act = act.trim();
            assertEquals("File content mismatch", exp, act);

            rowCompared++;

            exp = expectedR.readLine();
            act = actualR.readLine();
        }

        // Catch errors where only one of 'exp' and 'act' is null
        assertEquals("File content mismatch", exp, act);

        expectedR.close();
        actualR.close();

        return rowCompared;
    }

    /**
     * Reads file content as a string.
     *
     * @param   f  the file to read. Must be not <code>null</code>.
     * @return  the string representing
     * @throws  IOException  if the file <code>f</code> does not exist, is a
     *                       directory rather than a regular file, cannot be
     *                       opened for reading or an I/O error occurs.
     */
    private static String readFileAsString(final File f) throws IOException {
        BufferedReader file = null;

        try {
            StringBuilder buf = new StringBuilder();
            file = new BufferedReader(new FileReader(f));

            for (String s = file.readLine(); s != null; s = file.readLine()) {
                buf.append(s).append(LINE_SEPARATOR);
            }
            return buf.toString();
        } finally {
            if (file != null) {
                file.close();
            }
        }
    }
}
