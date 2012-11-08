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
import it.imolinfo.jbi4cics.locator.ServiceLocation;
import it.imolinfo.jbi4cics.locator.SimpleLocation;
import it.imolinfo.jbi4cics.messageformat.commarea.CommareaBeanMappingDescriptor;
import it.imolinfo.jbi4cics.test.BaseCommareaTest;
import it.imolinfo.jbi4cics.webservices.descriptor.ServiceDescriptor;
import it.imolinfo.jbi4cics.webservices.runtime.ServiceCreator;
import it.imolinfo.jbi4cics.webservices.utils.generators.ServiceBeanGenerator;
import it.imolinfo.jbi4cics.webservices.utils.generators.ServiceInterfaceGenerator;
import java.io.StringReader;
import java.util.Iterator;
import org.codehaus.xfire.XFire;
import org.codehaus.xfire.service.Service;
import org.codehaus.xfire.util.STAXUtils;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.filter.ElementFilter;
import antlr.ANTLRException;

public class TwoCopyCobolServiceInvocationTest extends BaseCommareaTest {

    public TwoCopyCobolServiceInvocationTest() {
    }

    public TwoCopyCobolServiceInvocationTest(String name) {
        super(name);
    }

    private static CommareaBeanMappingDescriptor parseCopyCobol(
            String copyCobol) throws ANTLRException, Jbi4cicsException {
        CommareaLexer lexer = new CommareaLexer(new StringReader(copyCobol));
        CommareaParser parser = new CommareaParser(lexer);

        return parser.commarea_definition();
    }

    private static Element getElementByName(Document doc, String elementName) {
        Iterator it = doc.getDescendants(new ElementFilter(elementName));

        assertTrue("Element " + elementName + " not found", it.hasNext());
        return (Element) it.next();
    }

    public void testTwoCopyCobolServiceInvocation() throws Exception {
        BCELClassLoader loader = new BCELClassLoader(
                Thread.currentThread().getContextClassLoader());
        XFire xfire = getXFire();
        ServiceDescriptor desc = new ServiceDescriptor();
        SimpleLocation simpleLocation = new SimpleLocation();
        Service service;
        Document response;

        desc.setOperationName("provaNestedOperation");
        desc.setServiceName("ProvaNested");
        desc.setServiceInterfacePackageName(
                "it.imolinfo.jbi4cics.test.webservices.utils.generators");
        desc.setServiceInterfaceName("ProvaNestedBeanInterface");
        desc.setServiceNameSpace(
                "urn:it.imolinfo.jbi4cics.test.webservices.utils.generators");
        desc.setInputBeanClassName("ProvaInputNestedBean");
        desc.setOutputBeanClassName("ProvaOutputNestedBean");
        desc.setInputMappingDescriptor(
                parseCopyCobol("02 CA-RETCODE1 PIC XXXXXXXXXX.\n"));
        desc.setOutputMappingDescriptor(parseCopyCobol(
                "02 CA-RETCODE1 PIC XXXX.\n"
                + "02 CA-RETCODE2 PIC X.\n"
                + "02 CA-RETCODE3 PIC XXXXX.\n"));
        desc.setCodePage("CP037");

        // aggiungo le info del servizio
        simpleLocation.setConnectionType(ServiceLocation.DUMMY);
        desc.setServiceLocation(simpleLocation);

        // creo i service bean
        new ServiceBeanGenerator(desc, true).generateBeanClass(loader);
        new ServiceBeanGenerator(desc, false).generateBeanClass(loader);

        // creo la service interface
        new ServiceInterfaceGenerator(desc).generateServiceInterface(loader);

        // creo il servizio xfire
        service = new ServiceCreator().createService(desc, xfire);
        xfire.getServiceRegistry().register(service);

        // invoco il servizio
        response = invokeService(service.getName().getLocalPart(),
                STAXUtils.createXMLStreamReader(new StringReader(
                        "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:urn=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\" xmlns:gen=\"http://generators.utils.webservices.test.jbi4cics.imolinfo.it\">\n"
                        + "   <soapenv:Body>\n"
                        + "      <urn:provaNestedOperation>\n"
                        + "         <urn:in0>\n"
                        + "            <gen:CA_RETCODE1>Ciao mondo</gen:CA_RETCODE1>\n"
                        + "         </urn:in0>\n"
                        + "      </urn:provaNestedOperation>\n"
                        + "   </soapenv:Body>\n"
                        + "</soapenv:Envelope>\n")));
        // printNode(response);
        addNamespace("test",
                "urn:it.imolinfo.jbi4cics.test.webservices.utils.generators");
        assertValid("//test:provaNestedOperationResponse", response);

        assertEquals("Wrong element content", "Ciao",
                     getElementByName(response, "CA_RETCODE1").getText());
        assertEquals("Wrong element content", "",
                     getElementByName(response, "CA_RETCODE2").getText());
        assertEquals("Wrong element content", "mondo",
                     getElementByName(response, "CA_RETCODE3").getText());
    }

    public void testTwoCopyCobolServiceInvocationBis() throws Exception {
        BCELClassLoader loader = new BCELClassLoader(
                Thread.currentThread().getContextClassLoader());
        XFire xfire = getXFire();
        ServiceDescriptor desc = new ServiceDescriptor();
        SimpleLocation simpleLocation = new SimpleLocation();
        Service service;
        Document response;

        desc.setOperationName("provaNestedOperation");
        desc.setServiceName("ProvaNested");
        desc.setServiceInterfacePackageName(
                "it.imolinfo.jbi4cics.test.webservices.utils.generators");
        desc.setServiceInterfaceName("ProvaNestedBeanInterface");
        desc.setServiceNameSpace(
                "urn:it.imolinfo.jbi4cics.test.webservices.utils.generators");
        desc.setInputBeanClassName("ProvaInputNestedBean");
        desc.setOutputBeanClassName("ProvaOutputNestedBean");
        desc.setInputMappingDescriptor(
                parseCopyCobol("01 MY-ITEM PIC 99999999 .\n"));
        desc.setOutputMappingDescriptor(parseCopyCobol(
                "  01 MY-ITEM1 PIC 99.\n"
                + "01 MY-ITEM2 PIC 999999.\n"));
        desc.setCodePage("CP037");

        // aggiungo le info del servizio
        simpleLocation.setConnectionType(ServiceLocation.DUMMY);
        desc.setServiceLocation(simpleLocation);

        // creo i service bean
        new ServiceBeanGenerator(desc, true).generateBeanClass(loader);
        new ServiceBeanGenerator(desc, false).generateBeanClass(loader);

        // creo la service interface
        new ServiceInterfaceGenerator(desc).generateServiceInterface(loader);

        // creo il servizio xfire
        service = new ServiceCreator().createService(desc, xfire);
        xfire.getServiceRegistry().register(service);

        // invoco il servizio
        response = invokeService(service.getName().getLocalPart(),
                STAXUtils.createXMLStreamReader(new StringReader(
                        "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:urn=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\" xmlns:gen=\"http://generators.utils.webservices.test.jbi4cics.imolinfo.it\">\n"
                        + "   <soapenv:Body>\n"
                        + "      <urn:provaNestedOperation>\n"
                        + "         <urn:in0>\n"
                        + "            <gen:MY_ITEM>12345678</gen:MY_ITEM>\n"
                        + "         </urn:in0>\n"
                        + "      </urn:provaNestedOperation>\n"
                        + "   </soapenv:Body>\n"
                        + "</soapenv:Envelope>\n")));
//        printNode(response);

        assertEquals("Wrong element content", "12",
                     getElementByName(response, "MY_ITEM1").getText());
        assertEquals("Wrong element content", "345678",
                     getElementByName(response, "MY_ITEM2").getText());
    }
}
