/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.test.webservices.runtime;

import it.imolinfo.jbi4cics.commareaparser.CommareaLexer;
import it.imolinfo.jbi4cics.commareaparser.CommareaParser;
import it.imolinfo.jbi4cics.exception.Jbi4cicsException;
import it.imolinfo.jbi4cics.jbi.BCELClassLoader;
import it.imolinfo.jbi4cics.messageformat.commarea.CommareaBeanMappingDescriptor;
import it.imolinfo.jbi4cics.test.BaseCommareaTest;
import it.imolinfo.jbi4cics.webservices.descriptor.ServiceDescriptor;
import it.imolinfo.jbi4cics.webservices.runtime.ServiceCreator;
import it.imolinfo.jbi4cics.webservices.utils.generators.ServiceBeanGenerator;
import it.imolinfo.jbi4cics.webservices.utils.generators.ServiceInterfaceGenerator;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import org.codehaus.xfire.XFireFactory;
import antlr.ANTLRException;

public class TwoCopyCobolServiceCreatorTest extends BaseCommareaTest {

    public TwoCopyCobolServiceCreatorTest() {
    }

    public TwoCopyCobolServiceCreatorTest(String name) {
        super(name);
    }

    public void testTwoCopyCobolServiceCreator() throws Exception {
        BCELClassLoader loader = new BCELClassLoader(
                Thread.currentThread().getContextClassLoader());
        ServiceDescriptor desc = new ServiceDescriptor();

        desc.setOperationName("provaOperation");
        desc.setServiceName("ProvaService");
        desc.setServiceInterfacePackageName(
                "it.imolinfo.jbi4cics.test.webservices.utils.generators");
        desc.setServiceInterfaceName("ProvaServiceBeanInterface");
        desc.setServiceNameSpace(
                "urn:it.imolinfo.jbi4cics.test.webservices.utils.generators");
        desc.setInputBeanClassName("ProvaInputServiceBean");
        desc.setOutputBeanClassName("ProvaOutputServiceBean");
        desc.setInputMappingDescriptor(
                parseCopyCobolFile(testRootCommarea + "/NestedCommarea.txt"));
        desc.setOutputMappingDescriptor(
                parseCopyCobolFile(testRootCommarea + "/OccursCommarea.txt"));

        // Create input service bean
        new ServiceBeanGenerator(desc, true).generateBeanClass(loader);

        // Create output service bean
        new ServiceBeanGenerator(desc, false).generateBeanClass(loader);

        // Create service interface
        new ServiceInterfaceGenerator(desc).generateServiceInterface(loader);

        // Creates the XFire service
        new ServiceCreator().createService(
                desc, XFireFactory.newInstance().getXFire());

        assertTrue("It's all OK, we're here without exceptions!", true);
    }

    private static CommareaBeanMappingDescriptor parseCopyCobolFile(
            String fileName)
            throws FileNotFoundException, ANTLRException, Jbi4cicsException {
        InputStream is = new FileInputStream(fileName);
        CommareaLexer lexer = new CommareaLexer(is);
        CommareaParser parser = new CommareaParser(lexer);

        return parser.commarea_definition();
    }
}
