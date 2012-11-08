/*
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.test.webservices.runtime;

import it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsAddress;
import it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsBinding;
import it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension;
import java.io.IOException;
import java.io.StringReader;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import junit.framework.TestCase;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import com.ibm.wsdl.Constants;

public class WSDLDeserializerTest extends TestCase {

    private WSDLReader reader;

    public WSDLDeserializerTest() {
    }
    
    public WSDLDeserializerTest(String name) {
        super(name);
    }
    
    private static Document toDocument(String s)
            throws SAXException, IOException, ParserConfigurationException {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder;
        
        factory.setNamespaceAware(true);
        builder = factory.newDocumentBuilder();
        return builder.parse(new InputSource(new StringReader(s)));
    }

    private static Jbi4CicsAddress getCicsAddress(Definition def) {
        for (Object i : def.getServices().values()) {
            Service svc = (Service) i;
            
            for (Object j : svc.getPorts().values()) {
                Port port = (Port) j;
                
                for (Object k : port.getExtensibilityElements()) {
                    if (k.getClass() == Jbi4CicsAddress.class) {
                        return (Jbi4CicsAddress) k;
                    }
                }
            }
        }
        return null;
    }
    
    private static Jbi4CicsBinding getCicsBinding(Definition def) {
        for (Object i : def.getServices().values()) {
            Service svc = (Service) i;
            
            for (Object j : svc.getPorts().values()) {
                Port port = (Port) j;
                
                for (Object k : port.getBinding().getExtensibilityElements()) {
                    if (k.getClass() == Jbi4CicsBinding.class) {
                        return (Jbi4CicsBinding) k;
                    }
                }
            }
        }
        return null;
    }
    
    @Override
    protected void setUp() throws Exception {
        WSDLFactory factory;
        ExtensionRegistry registry;

        super.setUp();

        factory = WSDLFactory.newInstance();
        registry = factory.newPopulatedExtensionRegistry();
        reader = factory.newWSDLReader();
        reader.setFeature(Constants.FEATURE_VERBOSE, false);
        reader.setFeature(Constants.FEATURE_IMPORT_DOCUMENTS, true);
        Jbi4CicsExtension.register(registry);
        reader.setExtensionRegistry(registry);
    }

    public void testDeserializeAddressAndBinding() throws Exception {
        Document wsdl = toDocument(
                  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                + "<wsdl:definitions xmlns:imolacics=\"uri://schemas.imola.it/jbi/wsdl-extensions/cics/\" targetNamespace=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\" xmlns:tns=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\" xmlns:wsdlsoap=\"http://schemas.xmlsoap.org/wsdl/soap/\" xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:ns1=\"http://generators.utils.webservices.test.jbi4cics.imolinfo.it\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soapenc11=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:soapenc12=\"http://www.w3.org/2003/05/soap-encoding\" xmlns:soap11=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:wsdl=\"http://schemas.xmlsoap.org/wsdl/\">\n"
                + "  <wsdl:types>\n"
                + "    <xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" attributeFormDefault=\"qualified\" elementFormDefault=\"qualified\" targetNamespace=\"http://generators.utils.webservices.test.jbi4cics.imolinfo.it\">\n"
                + "      <xsd:complexType name=\"ProvaEchoBean\">\n"
                + "      <xsd:sequence>\n"
                + "          <xsd:element minOccurs=\"0\" name=\"string\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "        </xsd:sequence>\n"
                + "      </xsd:complexType>\n"
                + "    </xsd:schema>\n"
                + "    <xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" attributeFormDefault=\"qualified\" elementFormDefault=\"qualified\" targetNamespace=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\">\n"
                + "      <xsd:element name=\"provaEchoOperation\">\n"
                + "        <xsd:complexType>\n"
                + "          <xsd:sequence>\n"
                + "            <xsd:element maxOccurs=\"1\" minOccurs=\"1\" name=\"in0\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "          </xsd:sequence>\n"
                + "        </xsd:complexType>\n"
                + "      </xsd:element>\n"
                + "      <xsd:element name=\"provaEchoOperationResponse\">\n"
                + "        <xsd:complexType>\n"
                + "          <xsd:sequence>\n"
                + "            <xsd:element maxOccurs=\"1\" minOccurs=\"1\" name=\"out\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "          </xsd:sequence>\n"
                + "        </xsd:complexType>\n"
                + "      </xsd:element>\n"
                + "    </xsd:schema>\n"
                + "  </wsdl:types>\n"
                + "  <wsdl:message name=\"provaEchoOperationRequest\">\n"
                + "    <wsdl:part name=\"parameters\" element=\"tns:provaEchoOperation\"/>\n"
                + "  </wsdl:message>\n"
                + "  <wsdl:message name=\"provaEchoOperationResponse\">\n"
                + "    <wsdl:part name=\"parameters\" element=\"tns:provaEchoOperationResponse\"/>\n"
                + "  </wsdl:message>\n"
                + "  <wsdl:portType name=\"ProvaEchoPortType\">\n"
                + "    <wsdl:operation name=\"provaEchoOperation\">\n"
                + "      <wsdl:input name=\"provaEchoOperationRequest\" message=\"tns:provaEchoOperationRequest\"/>\n"
                + "      <wsdl:output name=\"provaEchoOperationResponse\" message=\"tns:provaEchoOperationResponse\"/>\n"
                + "    </wsdl:operation>\n"
                + "  </wsdl:portType>\n"
                + "  <wsdl:binding name=\"ProvaEchoHttpBinding\" type=\"tns:ProvaEchoPortType\">\n"
                + "    <imolacics:binding servicePackageName=\"it.imolinfo.jbi4cics.test\" codePage=\"CP037\" sameCopyCobol=\"false\">\n"
                + "      <imolacics:copyCobol>\n"
                + "        02 CA-RETCODE1 PIC XXXXX        .\n"
                + "        02 CA-RETCODE2                  .\n"
                + "      </imolacics:copyCobol>\n"
                + "      <imolacics:outputCopyCobol>\n"
                + "        03 CA-RETCODE3 PIC 999.\n"
                + "        03 CA-RETCODE4 PIC XX.\n"
                + "      </imolacics:outputCopyCobol>\n"
                + "    </imolacics:binding>\n"
                + "    <wsdl:operation name=\"provaEchoOperation\">\n"
                + "      <wsdl:input name=\"provaEchoOperationRequest\"/>\n"
                + "      <wsdl:output name=\"provaEchoOperationResponse\"/>\n"
                + "    </wsdl:operation>\n"
                + "  </wsdl:binding>\n"
                + "  <wsdl:service name=\"ProvaEcho\">\n"
                + "    <wsdl:port name=\"ProvaEchoHttpPort\" binding=\"tns:ProvaEchoHttpBinding\">\n"
                + "      <imolacics:address username=\"john\" password=\"pwd\" connectionType=\"CICS\" JNDIConnectionName=\"jndiName\" programName=\"main\" transactionName=\"imola\" tpn=\"false\"/>\n"
                + "    </wsdl:port>\n"
                + "  </wsdl:service>\n"
                + "</wsdl:definitions>\n");
        Definition def = reader.readWSDL(null, wsdl);
        Jbi4CicsAddress expectedAddress = new Jbi4CicsAddress();
        Jbi4CicsBinding expectedBinding = new Jbi4CicsBinding();
        
        expectedAddress.setConnectionType("CICS");
        expectedAddress.setElementType(new QName(
                "uri://schemas.imola.it/jbi/wsdl-extensions/cics/", "address"));
        expectedAddress.setJNDIConnectionName("jndiName");
        expectedAddress.setPassword("pwd");
        expectedAddress.setProgramName("main");
        expectedAddress.setTpn(false);
        expectedAddress.setTransactionName("imola");
        expectedAddress.setUsername("john");

        expectedBinding.setCodePage("CP037");
        expectedBinding.setCopyCobol(
                "\n" 
                + "        02 CA-RETCODE1 PIC XXXXX        .\n"
                + "        02 CA-RETCODE2                  .\n"
                + "      ");
        expectedBinding.setElementType(new QName(
                "uri://schemas.imola.it/jbi/wsdl-extensions/cics/", "binding"));
        expectedBinding.setOutputCopyCobol(
                "\n" 
                + "        03 CA-RETCODE3 PIC 999.\n"
                + "        03 CA-RETCODE4 PIC XX.\n"
                + "      ");
        expectedBinding.setSameCopyCobol(Boolean.FALSE);
        expectedBinding.setServicePackageName("it.imolinfo.jbi4cics.test");
        
        assertEquals("Received address doesn't match the expected address",
                     expectedAddress, getCicsAddress(def));
        assertEquals("Received binding doesn't match the expected binding",
                     expectedBinding, getCicsBinding(def));
    }

    public void testBindingWithoutRequiredOutputCopyCobol() throws Exception {
        Document wsdl = toDocument(
                  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                + "<wsdl:definitions xmlns:imolacics=\"uri://schemas.imola.it/jbi/wsdl-extensions/cics/\" targetNamespace=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\" xmlns:tns=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\" xmlns:wsdlsoap=\"http://schemas.xmlsoap.org/wsdl/soap/\" xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:ns1=\"http://generators.utils.webservices.test.jbi4cics.imolinfo.it\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soapenc11=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:soapenc12=\"http://www.w3.org/2003/05/soap-encoding\" xmlns:soap11=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:wsdl=\"http://schemas.xmlsoap.org/wsdl/\">\n"
                + "  <wsdl:types>\n"
                + "    <xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" attributeFormDefault=\"qualified\" elementFormDefault=\"qualified\" targetNamespace=\"http://generators.utils.webservices.test.jbi4cics.imolinfo.it\">\n"
                + "      <xsd:complexType name=\"ProvaEchoBean\">\n"
                + "      <xsd:sequence>\n"
                + "          <xsd:element minOccurs=\"0\" name=\"string\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "        </xsd:sequence>\n"
                + "      </xsd:complexType>\n"
                + "    </xsd:schema>\n"
                + "    <xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" attributeFormDefault=\"qualified\" elementFormDefault=\"qualified\" targetNamespace=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\">\n"
                + "      <xsd:element name=\"provaEchoOperation\">\n"
                + "        <xsd:complexType>\n"
                + "          <xsd:sequence>\n"
                + "            <xsd:element maxOccurs=\"1\" minOccurs=\"1\" name=\"in0\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "          </xsd:sequence>\n"
                + "        </xsd:complexType>\n"
                + "      </xsd:element>\n"
                + "      <xsd:element name=\"provaEchoOperationResponse\">\n"
                + "        <xsd:complexType>\n"
                + "          <xsd:sequence>\n"
                + "            <xsd:element maxOccurs=\"1\" minOccurs=\"1\" name=\"out\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "          </xsd:sequence>\n"
                + "        </xsd:complexType>\n"
                + "      </xsd:element>\n"
                + "    </xsd:schema>\n"
                + "  </wsdl:types>\n"
                + "  <wsdl:message name=\"provaEchoOperationRequest\">\n"
                + "    <wsdl:part name=\"parameters\" element=\"tns:provaEchoOperation\"/>\n"
                + "  </wsdl:message>\n"
                + "  <wsdl:message name=\"provaEchoOperationResponse\">\n"
                + "    <wsdl:part name=\"parameters\" element=\"tns:provaEchoOperationResponse\"/>\n"
                + "  </wsdl:message>\n"
                + "  <wsdl:portType name=\"ProvaEchoPortType\">\n"
                + "    <wsdl:operation name=\"provaEchoOperation\">\n"
                + "      <wsdl:input name=\"provaEchoOperationRequest\" message=\"tns:provaEchoOperationRequest\"/>\n"
                + "      <wsdl:output name=\"provaEchoOperationResponse\" message=\"tns:provaEchoOperationResponse\"/>\n"
                + "    </wsdl:operation>\n"
                + "  </wsdl:portType>\n"
                + "  <wsdl:binding name=\"ProvaEchoHttpBinding\" type=\"tns:ProvaEchoPortType\">\n"
                + "    <imolacics:binding servicePackageName=\"it.imolinfo.jbi4cics.test\" codePage=\"CP037\" sameCopyCobol=\"FALSE\">\n"
                + "      <imolacics:copyCobol>\n"
                + "        02 CA-RETCODE1 PIC XXXXX        .\n"
                + "        02 CA-RETCODE2                  .\n"
                + "      </imolacics:copyCobol>\n"
                + "    </imolacics:binding>\n"
                + "    <wsdl:operation name=\"provaEchoOperation\">\n"
                + "      <wsdl:input name=\"provaEchoOperationRequest\"/>\n"
                + "      <wsdl:output name=\"provaEchoOperationResponse\"/>\n"
                + "    </wsdl:operation>\n"
                + "  </wsdl:binding>\n"
                + "  <wsdl:service name=\"ProvaEcho\">\n"
                + "    <wsdl:port name=\"ProvaEchoHttpPort\" binding=\"tns:ProvaEchoHttpBinding\">\n"
                + "      <imolacics:address username=\"john\" password=\"pwd\" connectionType=\"CICS\" JNDIConnectionName=\"jndiName\" programName=\"main\" transactionName=\"imola\" tpn=\"false\"/>\n"
                + "    </wsdl:port>\n"
                + "  </wsdl:service>\n"
                + "</wsdl:definitions>\n");
        
        try {
            reader.readWSDL(null, wsdl);
            fail("Parsed a WSDL without the required output copy Cobol");
        } catch (WSDLException e) {
            assertTrue("Wrong error while parsing an incorrect WSDL", 
                       e.getLocalizedMessage().indexOf("CIC001315:") > 0);
        }
    }

    public void testBindingWithOptionalOutputCopyCobol() throws Exception {
        Document wsdl = toDocument(
                  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                + "<wsdl:definitions xmlns:imolacics=\"uri://schemas.imola.it/jbi/wsdl-extensions/cics/\" targetNamespace=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\" xmlns:tns=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\" xmlns:wsdlsoap=\"http://schemas.xmlsoap.org/wsdl/soap/\" xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:ns1=\"http://generators.utils.webservices.test.jbi4cics.imolinfo.it\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soapenc11=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:soapenc12=\"http://www.w3.org/2003/05/soap-encoding\" xmlns:soap11=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:wsdl=\"http://schemas.xmlsoap.org/wsdl/\">\n"
                + "  <wsdl:types>\n"
                + "    <xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" attributeFormDefault=\"qualified\" elementFormDefault=\"qualified\" targetNamespace=\"http://generators.utils.webservices.test.jbi4cics.imolinfo.it\">\n"
                + "      <xsd:complexType name=\"ProvaEchoBean\">\n"
                + "      <xsd:sequence>\n"
                + "          <xsd:element minOccurs=\"0\" name=\"string\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "        </xsd:sequence>\n"
                + "      </xsd:complexType>\n"
                + "    </xsd:schema>\n"
                + "    <xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" attributeFormDefault=\"qualified\" elementFormDefault=\"qualified\" targetNamespace=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\">\n"
                + "      <xsd:element name=\"provaEchoOperation\">\n"
                + "        <xsd:complexType>\n"
                + "          <xsd:sequence>\n"
                + "            <xsd:element maxOccurs=\"1\" minOccurs=\"1\" name=\"in0\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "          </xsd:sequence>\n"
                + "        </xsd:complexType>\n"
                + "      </xsd:element>\n"
                + "      <xsd:element name=\"provaEchoOperationResponse\">\n"
                + "        <xsd:complexType>\n"
                + "          <xsd:sequence>\n"
                + "            <xsd:element maxOccurs=\"1\" minOccurs=\"1\" name=\"out\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "          </xsd:sequence>\n"
                + "        </xsd:complexType>\n"
                + "      </xsd:element>\n"
                + "    </xsd:schema>\n"
                + "  </wsdl:types>\n"
                + "  <wsdl:message name=\"provaEchoOperationRequest\">\n"
                + "    <wsdl:part name=\"parameters\" element=\"tns:provaEchoOperation\"/>\n"
                + "  </wsdl:message>\n"
                + "  <wsdl:message name=\"provaEchoOperationResponse\">\n"
                + "    <wsdl:part name=\"parameters\" element=\"tns:provaEchoOperationResponse\"/>\n"
                + "  </wsdl:message>\n"
                + "  <wsdl:portType name=\"ProvaEchoPortType\">\n"
                + "    <wsdl:operation name=\"provaEchoOperation\">\n"
                + "      <wsdl:input name=\"provaEchoOperationRequest\" message=\"tns:provaEchoOperationRequest\"/>\n"
                + "      <wsdl:output name=\"provaEchoOperationResponse\" message=\"tns:provaEchoOperationResponse\"/>\n"
                + "    </wsdl:operation>\n"
                + "  </wsdl:portType>\n"
                + "  <wsdl:binding name=\"ProvaEchoHttpBinding\" type=\"tns:ProvaEchoPortType\">\n"
                + "    <imolacics:binding servicePackageName=\"it.imolinfo.jbi4cics.test\" codePage=\"CP037\" sameCopyCobol=\"true\">\n"
                + "      <imolacics:copyCobol>\n"
                + "        02 CA-RETCODE1 PIC XXXXX        .\n"
                + "        02 CA-RETCODE2                  .\n"
                + "      </imolacics:copyCobol>\n"
                + "      <imolacics:outputCopyCobol>\n"
                + "        03 CA-RETCODE3 PIC 999.\n"
                + "        03 CA-RETCODE4 PIC XX.\n"
                + "      </imolacics:outputCopyCobol>\n"
                + "    </imolacics:binding>\n"
                + "    <wsdl:operation name=\"provaEchoOperation\">\n"
                + "      <wsdl:input name=\"provaEchoOperationRequest\"/>\n"
                + "      <wsdl:output name=\"provaEchoOperationResponse\"/>\n"
                + "    </wsdl:operation>\n"
                + "  </wsdl:binding>\n"
                + "  <wsdl:service name=\"ProvaEcho\">\n"
                + "    <wsdl:port name=\"ProvaEchoHttpPort\" binding=\"tns:ProvaEchoHttpBinding\">\n"
                + "      <imolacics:address username=\"john\" password=\"pwd\" connectionType=\"CICS\" JNDIConnectionName=\"jndiName\" programName=\"main\" transactionName=\"imola\" tpn=\"false\"/>\n"
                + "    </wsdl:port>\n"
                + "  </wsdl:service>\n"
                + "</wsdl:definitions>\n");
        Definition def = reader.readWSDL(null, wsdl);
        Jbi4CicsAddress expectedAddress = new Jbi4CicsAddress();
        Jbi4CicsBinding expectedBinding = new Jbi4CicsBinding();
        
        expectedAddress.setConnectionType("CICS");
        expectedAddress.setElementType(new QName(
                "uri://schemas.imola.it/jbi/wsdl-extensions/cics/", "address"));
        expectedAddress.setJNDIConnectionName("jndiName");
        expectedAddress.setPassword("pwd");
        expectedAddress.setProgramName("main");
        expectedAddress.setTpn(false);
        expectedAddress.setTransactionName("imola");
        expectedAddress.setUsername("john");

        expectedBinding.setCodePage("CP037");
        expectedBinding.setCopyCobol(
                "\n" 
                + "        02 CA-RETCODE1 PIC XXXXX        .\n"
                + "        02 CA-RETCODE2                  .\n"
                + "      ");
        expectedBinding.setElementType(new QName(
                "uri://schemas.imola.it/jbi/wsdl-extensions/cics/", "binding"));
        expectedBinding.setOutputCopyCobol(
                "\n" 
                + "        03 CA-RETCODE3 PIC 999.\n"
                + "        03 CA-RETCODE4 PIC XX.\n"
                + "      ");
        expectedBinding.setSameCopyCobol(Boolean.TRUE);
        expectedBinding.setServicePackageName("it.imolinfo.jbi4cics.test");
        
        assertEquals("Received address doesn't match the expected address",
                     expectedAddress, getCicsAddress(def));
        assertEquals("Received binding doesn't match the expected binding",
                     expectedBinding, getCicsBinding(def));
    }
    
    public void testBindingWithTooManyCopyCobol() throws Exception {
        Document wsdl = toDocument(
                  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                + "<wsdl:definitions xmlns:imolacics=\"uri://schemas.imola.it/jbi/wsdl-extensions/cics/\" targetNamespace=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\" xmlns:tns=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\" xmlns:wsdlsoap=\"http://schemas.xmlsoap.org/wsdl/soap/\" xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:ns1=\"http://generators.utils.webservices.test.jbi4cics.imolinfo.it\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soapenc11=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:soapenc12=\"http://www.w3.org/2003/05/soap-encoding\" xmlns:soap11=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:wsdl=\"http://schemas.xmlsoap.org/wsdl/\">\n"
                + "  <wsdl:types>\n"
                + "    <xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" attributeFormDefault=\"qualified\" elementFormDefault=\"qualified\" targetNamespace=\"http://generators.utils.webservices.test.jbi4cics.imolinfo.it\">\n"
                + "      <xsd:complexType name=\"ProvaEchoBean\">\n"
                + "      <xsd:sequence>\n"
                + "          <xsd:element minOccurs=\"0\" name=\"string\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "        </xsd:sequence>\n"
                + "      </xsd:complexType>\n"
                + "    </xsd:schema>\n"
                + "    <xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" attributeFormDefault=\"qualified\" elementFormDefault=\"qualified\" targetNamespace=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\">\n"
                + "      <xsd:element name=\"provaEchoOperation\">\n"
                + "        <xsd:complexType>\n"
                + "          <xsd:sequence>\n"
                + "            <xsd:element maxOccurs=\"1\" minOccurs=\"1\" name=\"in0\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "          </xsd:sequence>\n"
                + "        </xsd:complexType>\n"
                + "      </xsd:element>\n"
                + "      <xsd:element name=\"provaEchoOperationResponse\">\n"
                + "        <xsd:complexType>\n"
                + "          <xsd:sequence>\n"
                + "            <xsd:element maxOccurs=\"1\" minOccurs=\"1\" name=\"out\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "          </xsd:sequence>\n"
                + "        </xsd:complexType>\n"
                + "      </xsd:element>\n"
                + "    </xsd:schema>\n"
                + "  </wsdl:types>\n"
                + "  <wsdl:message name=\"provaEchoOperationRequest\">\n"
                + "    <wsdl:part name=\"parameters\" element=\"tns:provaEchoOperation\"/>\n"
                + "  </wsdl:message>\n"
                + "  <wsdl:message name=\"provaEchoOperationResponse\">\n"
                + "    <wsdl:part name=\"parameters\" element=\"tns:provaEchoOperationResponse\"/>\n"
                + "  </wsdl:message>\n"
                + "  <wsdl:portType name=\"ProvaEchoPortType\">\n"
                + "    <wsdl:operation name=\"provaEchoOperation\">\n"
                + "      <wsdl:input name=\"provaEchoOperationRequest\" message=\"tns:provaEchoOperationRequest\"/>\n"
                + "      <wsdl:output name=\"provaEchoOperationResponse\" message=\"tns:provaEchoOperationResponse\"/>\n"
                + "    </wsdl:operation>\n"
                + "  </wsdl:portType>\n"
                + "  <wsdl:binding name=\"ProvaEchoHttpBinding\" type=\"tns:ProvaEchoPortType\">\n"
                + "    <imolacics:binding servicePackageName=\"it.imolinfo.jbi4cics.test\" codePage=\"CP037\" sameCopyCobol=\"false\">\n"
                + "      <imolacics:copyCobol>\n"
                + "        02 CA-RETCODE1 PIC XXXXX        .\n"
                + "        02 CA-RETCODE2                  .\n"
                + "      </imolacics:copyCobol>\n"
                + "      <imolacics:copyCobol>\n"
                + "        03 CA-RETCODE3 PIC 999.\n"
                + "        03 CA-RETCODE4 PIC XX.\n"
                + "      </imolacics:copyCobol>\n"
                + "    </imolacics:binding>\n"
                + "    <wsdl:operation name=\"provaEchoOperation\">\n"
                + "      <wsdl:input name=\"provaEchoOperationRequest\"/>\n"
                + "      <wsdl:output name=\"provaEchoOperationResponse\"/>\n"
                + "    </wsdl:operation>\n"
                + "  </wsdl:binding>\n"
                + "  <wsdl:service name=\"ProvaEcho\">\n"
                + "    <wsdl:port name=\"ProvaEchoHttpPort\" binding=\"tns:ProvaEchoHttpBinding\">\n"
                + "      <imolacics:address username=\"john\" password=\"pwd\" connectionType=\"CICS\" JNDIConnectionName=\"jndiName\" programName=\"main\" transactionName=\"imola\" tpn=\"false\"/>\n"
                + "    </wsdl:port>\n"
                + "  </wsdl:service>\n"
                + "</wsdl:definitions>\n");
        
        try {
            reader.readWSDL(null, wsdl);
            fail("Parsed a WSDL with two copy Cobol");
        } catch (WSDLException e) {
            assertTrue("Wrong error while parsing an incorrect WSDL", 
                       e.getLocalizedMessage().indexOf("CIC001314:") > 0);
        }
    }

    public void testBindingWithoutCopyCobol() throws Exception {
        Document wsdl = toDocument(
                  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                + "<wsdl:definitions xmlns:imolacics=\"uri://schemas.imola.it/jbi/wsdl-extensions/cics/\" targetNamespace=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\" xmlns:tns=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\" xmlns:wsdlsoap=\"http://schemas.xmlsoap.org/wsdl/soap/\" xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:ns1=\"http://generators.utils.webservices.test.jbi4cics.imolinfo.it\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soapenc11=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:soapenc12=\"http://www.w3.org/2003/05/soap-encoding\" xmlns:soap11=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:wsdl=\"http://schemas.xmlsoap.org/wsdl/\">\n"
                + "  <wsdl:types>\n"
                + "    <xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" attributeFormDefault=\"qualified\" elementFormDefault=\"qualified\" targetNamespace=\"http://generators.utils.webservices.test.jbi4cics.imolinfo.it\">\n"
                + "      <xsd:complexType name=\"ProvaEchoBean\">\n"
                + "      <xsd:sequence>\n"
                + "          <xsd:element minOccurs=\"0\" name=\"string\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "        </xsd:sequence>\n"
                + "      </xsd:complexType>\n"
                + "    </xsd:schema>\n"
                + "    <xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" attributeFormDefault=\"qualified\" elementFormDefault=\"qualified\" targetNamespace=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\">\n"
                + "      <xsd:element name=\"provaEchoOperation\">\n"
                + "        <xsd:complexType>\n"
                + "          <xsd:sequence>\n"
                + "            <xsd:element maxOccurs=\"1\" minOccurs=\"1\" name=\"in0\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "          </xsd:sequence>\n"
                + "        </xsd:complexType>\n"
                + "      </xsd:element>\n"
                + "      <xsd:element name=\"provaEchoOperationResponse\">\n"
                + "        <xsd:complexType>\n"
                + "          <xsd:sequence>\n"
                + "            <xsd:element maxOccurs=\"1\" minOccurs=\"1\" name=\"out\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "          </xsd:sequence>\n"
                + "        </xsd:complexType>\n"
                + "      </xsd:element>\n"
                + "    </xsd:schema>\n"
                + "  </wsdl:types>\n"
                + "  <wsdl:message name=\"provaEchoOperationRequest\">\n"
                + "    <wsdl:part name=\"parameters\" element=\"tns:provaEchoOperation\"/>\n"
                + "  </wsdl:message>\n"
                + "  <wsdl:message name=\"provaEchoOperationResponse\">\n"
                + "    <wsdl:part name=\"parameters\" element=\"tns:provaEchoOperationResponse\"/>\n"
                + "  </wsdl:message>\n"
                + "  <wsdl:portType name=\"ProvaEchoPortType\">\n"
                + "    <wsdl:operation name=\"provaEchoOperation\">\n"
                + "      <wsdl:input name=\"provaEchoOperationRequest\" message=\"tns:provaEchoOperationRequest\"/>\n"
                + "      <wsdl:output name=\"provaEchoOperationResponse\" message=\"tns:provaEchoOperationResponse\"/>\n"
                + "    </wsdl:operation>\n"
                + "  </wsdl:portType>\n"
                + "  <wsdl:binding name=\"ProvaEchoHttpBinding\" type=\"tns:ProvaEchoPortType\">\n"
                + "    <imolacics:binding servicePackageName=\"it.imolinfo.jbi4cics.test\" codePage=\"CP037\" sameCopyCobol=\"true\">\n"
                + "    </imolacics:binding>\n"
                + "    <wsdl:operation name=\"provaEchoOperation\">\n"
                + "      <wsdl:input name=\"provaEchoOperationRequest\"/>\n"
                + "      <wsdl:output name=\"provaEchoOperationResponse\"/>\n"
                + "    </wsdl:operation>\n"
                + "  </wsdl:binding>\n"
                + "  <wsdl:service name=\"ProvaEcho\">\n"
                + "    <wsdl:port name=\"ProvaEchoHttpPort\" binding=\"tns:ProvaEchoHttpBinding\">\n"
                + "      <imolacics:address username=\"john\" password=\"pwd\" connectionType=\"CICS\" JNDIConnectionName=\"jndiName\" programName=\"main\" transactionName=\"imola\" tpn=\"false\"/>\n"
                + "    </wsdl:port>\n"
                + "  </wsdl:service>\n"
                + "</wsdl:definitions>\n");
        
        try {
            reader.readWSDL(null, wsdl);
            fail("Parsed a WSDL without copy Cobol");
        } catch (WSDLException e) {
            assertTrue("Wrong error while parsing an incorrect WSDL", 
                       e.getLocalizedMessage().indexOf("CIC001309:") > 0);
        }
    }

    public void testBindingWithTooManyOutputCopyCobol() throws Exception {
        Document wsdl = toDocument(
                  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                + "<wsdl:definitions xmlns:imolacics=\"uri://schemas.imola.it/jbi/wsdl-extensions/cics/\" targetNamespace=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\" xmlns:tns=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\" xmlns:wsdlsoap=\"http://schemas.xmlsoap.org/wsdl/soap/\" xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:ns1=\"http://generators.utils.webservices.test.jbi4cics.imolinfo.it\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soapenc11=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:soapenc12=\"http://www.w3.org/2003/05/soap-encoding\" xmlns:soap11=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:wsdl=\"http://schemas.xmlsoap.org/wsdl/\">\n"
                + "  <wsdl:types>\n"
                + "    <xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" attributeFormDefault=\"qualified\" elementFormDefault=\"qualified\" targetNamespace=\"http://generators.utils.webservices.test.jbi4cics.imolinfo.it\">\n"
                + "      <xsd:complexType name=\"ProvaEchoBean\">\n"
                + "      <xsd:sequence>\n"
                + "          <xsd:element minOccurs=\"0\" name=\"string\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "        </xsd:sequence>\n"
                + "      </xsd:complexType>\n"
                + "    </xsd:schema>\n"
                + "    <xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" attributeFormDefault=\"qualified\" elementFormDefault=\"qualified\" targetNamespace=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\">\n"
                + "      <xsd:element name=\"provaEchoOperation\">\n"
                + "        <xsd:complexType>\n"
                + "          <xsd:sequence>\n"
                + "            <xsd:element maxOccurs=\"1\" minOccurs=\"1\" name=\"in0\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "          </xsd:sequence>\n"
                + "        </xsd:complexType>\n"
                + "      </xsd:element>\n"
                + "      <xsd:element name=\"provaEchoOperationResponse\">\n"
                + "        <xsd:complexType>\n"
                + "          <xsd:sequence>\n"
                + "            <xsd:element maxOccurs=\"1\" minOccurs=\"1\" name=\"out\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "          </xsd:sequence>\n"
                + "        </xsd:complexType>\n"
                + "      </xsd:element>\n"
                + "    </xsd:schema>\n"
                + "  </wsdl:types>\n"
                + "  <wsdl:message name=\"provaEchoOperationRequest\">\n"
                + "    <wsdl:part name=\"parameters\" element=\"tns:provaEchoOperation\"/>\n"
                + "  </wsdl:message>\n"
                + "  <wsdl:message name=\"provaEchoOperationResponse\">\n"
                + "    <wsdl:part name=\"parameters\" element=\"tns:provaEchoOperationResponse\"/>\n"
                + "  </wsdl:message>\n"
                + "  <wsdl:portType name=\"ProvaEchoPortType\">\n"
                + "    <wsdl:operation name=\"provaEchoOperation\">\n"
                + "      <wsdl:input name=\"provaEchoOperationRequest\" message=\"tns:provaEchoOperationRequest\"/>\n"
                + "      <wsdl:output name=\"provaEchoOperationResponse\" message=\"tns:provaEchoOperationResponse\"/>\n"
                + "    </wsdl:operation>\n"
                + "  </wsdl:portType>\n"
                + "  <wsdl:binding name=\"ProvaEchoHttpBinding\" type=\"tns:ProvaEchoPortType\">\n"
                + "    <imolacics:binding servicePackageName=\"it.imolinfo.jbi4cics.test\" codePage=\"CP037\" sameCopyCobol=\"true\">\n"
                + "      <imolacics:copyCobol>\n"
                + "        02 CA-RETCODE1 PIC XXXXX        .\n"
                + "        02 CA-RETCODE2                  .\n"
                + "      </imolacics:copyCobol>\n"
                + "      <imolacics:outputCopyCobol>\n"
                + "        03 CA-RETCODE3 PIC 999.\n"
                + "        03 CA-RETCODE4 PIC XX.\n"
                + "      </imolacics:outputCopyCobol>\n"
                + "      <imolacics:outputCopyCobol>\n"
                + "        03 CA-RETCODE3 PIC 999.\n"
                + "        03 CA-RETCODE4 PIC XX.\n"
                + "      </imolacics:outputCopyCobol>\n"
                + "    </imolacics:binding>\n"
                + "    <wsdl:operation name=\"provaEchoOperation\">\n"
                + "      <wsdl:input name=\"provaEchoOperationRequest\"/>\n"
                + "      <wsdl:output name=\"provaEchoOperationResponse\"/>\n"
                + "    </wsdl:operation>\n"
                + "  </wsdl:binding>\n"
                + "  <wsdl:service name=\"ProvaEcho\">\n"
                + "    <wsdl:port name=\"ProvaEchoHttpPort\" binding=\"tns:ProvaEchoHttpBinding\">\n"
                + "      <imolacics:address username=\"john\" password=\"pwd\" connectionType=\"CICS\" JNDIConnectionName=\"jndiName\" programName=\"main\" transactionName=\"imola\" tpn=\"false\"/>\n"
                + "    </wsdl:port>\n"
                + "  </wsdl:service>\n"
                + "</wsdl:definitions>\n");
        
        try {
            reader.readWSDL(null, wsdl);
            fail("Parsed a WSDL with two output copy Cobol");
        } catch (WSDLException e) {
            String msg =  e.getLocalizedMessage();
            
            assertTrue("Wrong error while parsing an incorrect WSDL", 
                       msg.indexOf("CIC001314:") > 0);
            assertTrue("Wrong error while parsing an incorrect WSDL", 
                       msg.indexOf("outputCopyCobol") > 0);
        }
    }

    public void testDefaultValueOfSameCopyCobolAttribute() throws Exception {
        Document wsdl = toDocument(
                  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                + "<wsdl:definitions xmlns:imolacics=\"uri://schemas.imola.it/jbi/wsdl-extensions/cics/\" targetNamespace=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\" xmlns:tns=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\" xmlns:wsdlsoap=\"http://schemas.xmlsoap.org/wsdl/soap/\" xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:ns1=\"http://generators.utils.webservices.test.jbi4cics.imolinfo.it\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soapenc11=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:soapenc12=\"http://www.w3.org/2003/05/soap-encoding\" xmlns:soap11=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:wsdl=\"http://schemas.xmlsoap.org/wsdl/\">\n"
                + "  <wsdl:types>\n"
                + "    <xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" attributeFormDefault=\"qualified\" elementFormDefault=\"qualified\" targetNamespace=\"http://generators.utils.webservices.test.jbi4cics.imolinfo.it\">\n"
                + "      <xsd:complexType name=\"ProvaEchoBean\">\n"
                + "      <xsd:sequence>\n"
                + "          <xsd:element minOccurs=\"0\" name=\"string\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "        </xsd:sequence>\n"
                + "      </xsd:complexType>\n"
                + "    </xsd:schema>\n"
                + "    <xsd:schema xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" attributeFormDefault=\"qualified\" elementFormDefault=\"qualified\" targetNamespace=\"urn:it.imolinfo.jbi4cics.test.webservices.utils.generators\">\n"
                + "      <xsd:element name=\"provaEchoOperation\">\n"
                + "        <xsd:complexType>\n"
                + "          <xsd:sequence>\n"
                + "            <xsd:element maxOccurs=\"1\" minOccurs=\"1\" name=\"in0\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "          </xsd:sequence>\n"
                + "        </xsd:complexType>\n"
                + "      </xsd:element>\n"
                + "      <xsd:element name=\"provaEchoOperationResponse\">\n"
                + "        <xsd:complexType>\n"
                + "          <xsd:sequence>\n"
                + "            <xsd:element maxOccurs=\"1\" minOccurs=\"1\" name=\"out\" nillable=\"true\" type=\"xsd:string\"/>\n"
                + "          </xsd:sequence>\n"
                + "        </xsd:complexType>\n"
                + "      </xsd:element>\n"
                + "    </xsd:schema>\n"
                + "  </wsdl:types>\n"
                + "  <wsdl:message name=\"provaEchoOperationRequest\">\n"
                + "    <wsdl:part name=\"parameters\" element=\"tns:provaEchoOperation\"/>\n"
                + "  </wsdl:message>\n"
                + "  <wsdl:message name=\"provaEchoOperationResponse\">\n"
                + "    <wsdl:part name=\"parameters\" element=\"tns:provaEchoOperationResponse\"/>\n"
                + "  </wsdl:message>\n"
                + "  <wsdl:portType name=\"ProvaEchoPortType\">\n"
                + "    <wsdl:operation name=\"provaEchoOperation\">\n"
                + "      <wsdl:input name=\"provaEchoOperationRequest\" message=\"tns:provaEchoOperationRequest\"/>\n"
                + "      <wsdl:output name=\"provaEchoOperationResponse\" message=\"tns:provaEchoOperationResponse\"/>\n"
                + "    </wsdl:operation>\n"
                + "  </wsdl:portType>\n"
                + "  <wsdl:binding name=\"ProvaEchoHttpBinding\" type=\"tns:ProvaEchoPortType\">\n"
                + "    <imolacics:binding servicePackageName=\"it.imolinfo.jbi4cics.test\" codePage=\"CP037\">\n"
                + "      <imolacics:copyCobol>\n"
                + "        02 CA-RETCODE1 PIC XXXXX        .\n"
                + "        02 CA-RETCODE2                  .\n"
                + "      </imolacics:copyCobol>\n"
                + "    </imolacics:binding>\n"
                + "    <wsdl:operation name=\"provaEchoOperation\">\n"
                + "      <wsdl:input name=\"provaEchoOperationRequest\"/>\n"
                + "      <wsdl:output name=\"provaEchoOperationResponse\"/>\n"
                + "    </wsdl:operation>\n"
                + "  </wsdl:binding>\n"
                + "  <wsdl:service name=\"ProvaEcho\">\n"
                + "    <wsdl:port name=\"ProvaEchoHttpPort\" binding=\"tns:ProvaEchoHttpBinding\">\n"
                + "      <imolacics:address username=\"john\" password=\"pwd\" connectionType=\"CICS\" JNDIConnectionName=\"jndiName\" programName=\"main\" transactionName=\"imola\" tpn=\"false\"/>\n"
                + "    </wsdl:port>\n"
                + "  </wsdl:service>\n"
                + "</wsdl:definitions>\n");
        Definition def = reader.readWSDL(null, wsdl);
        Jbi4CicsAddress expectedAddress = new Jbi4CicsAddress();
        Jbi4CicsBinding expectedBinding = new Jbi4CicsBinding();
        
        expectedAddress.setConnectionType("CICS");
        expectedAddress.setElementType(new QName(
                "uri://schemas.imola.it/jbi/wsdl-extensions/cics/", "address"));
        expectedAddress.setJNDIConnectionName("jndiName");
        expectedAddress.setPassword("pwd");
        expectedAddress.setProgramName("main");
        expectedAddress.setTpn(false);
        expectedAddress.setTransactionName("imola");
        expectedAddress.setUsername("john");

        expectedBinding.setCodePage("CP037");
        expectedBinding.setCopyCobol(
                "\n" 
                + "        02 CA-RETCODE1 PIC XXXXX        .\n"
                + "        02 CA-RETCODE2                  .\n"
                + "      ");
        expectedBinding.setElementType(new QName(
                "uri://schemas.imola.it/jbi/wsdl-extensions/cics/", "binding"));
        expectedBinding.setServicePackageName("it.imolinfo.jbi4cics.test");
        
        assertEquals("Received address doesn't match the expected address",
                     expectedAddress, getCicsAddress(def));
        assertEquals("Received binding doesn't match the expected binding",
                     expectedBinding, getCicsBinding(def));
    }
}
