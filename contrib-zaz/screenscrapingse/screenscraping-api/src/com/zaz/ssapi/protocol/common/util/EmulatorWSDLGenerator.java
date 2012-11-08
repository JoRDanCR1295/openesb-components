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
 * Copyright 2007-2008 ZAZ Consulting, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.zaz.ssapi.protocol.common.util;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.File;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

/**
 *
 * @author tianlize
 */
public class EmulatorWSDLGenerator {

    private static Document doc = null;
    private static Element definitions = null;

    public EmulatorWSDLGenerator() {
    }

    public static void generateWSDLFile(String name, File dir, String host, int port) {
        generateWSDL(host, port, name);
        writeXML(doc, dir, name + ".wsdl");
    }

    public static void writeXML(Document document, File dir, String fileName) {
        try {
            TransformerFactory tf = TransformerFactory.newInstance();
            tf.setAttribute("indent-number", new Integer(4));
            Transformer transformer = tf.newTransformer();
            DOMSource source = new DOMSource(document);
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            PrintWriter pw = new PrintWriter(new FileOutputStream(dir.getPath() + File.separator + "src" + File.separator + fileName));
            StreamResult result = new StreamResult(pw);
            transformer.transform(source, result);
        } catch (TransformerConfigurationException ex) {
            Logger.getLogger(EmulatorWSDLGenerator.class.getName()).log(Level.SEVERE, null, ex);
        } catch (TransformerException ex) {
            Logger.getLogger(EmulatorWSDLGenerator.class.getName()).log(Level.SEVERE, null, ex);
        } catch (FileNotFoundException ex) {
            Logger.getLogger(EmulatorWSDLGenerator.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private static void generateWSDL(String host, int port, String name) {
        initDocument();
        creatDefinitions(name);
        creatTypes(name);
        creatRequestMessage(name);
        creatResponseMessage(name);
        creatPortType(name);
        creatBinding(name);
        creatService(host, port, name);
        creatPartnerLink(name);
    }

    private static void initDocument() {
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        DocumentBuilder db = null;
        try {
            db = dbf.newDocumentBuilder();
        } catch (ParserConfigurationException pce) {
            pce.printStackTrace();
            System.exit(1);
        }
        doc = db.newDocument();
    }

    private static void creatDefinitions(String name) {
        definitions = doc.createElement("definitions");
        definitions.setAttribute("name", name);
        definitions.setAttribute("targetNamespace", "http://j2ee.netbeans.org/wsdl/" + name);
        definitions.setAttribute("xmlns", "http://schemas.xmlsoap.org/wsdl/");
        definitions.setAttribute("xmlns:wsdl", "http://schemas.xmlsoap.org/wsdl/");
        definitions.setAttribute("xmlns:xsd", "http://www.w3.org/2001/XMLSchema");
        definitions.setAttribute("xmlns:tns", "http://j2ee.netbeans.org/wsdl/" + name);
        definitions.setAttribute("xmlns:ns", "http://xml.netbeans.org/schema/" + name);
        definitions.setAttribute("xmlns:plnk", "http://docs.oasis-open.org/wsbpel/2.0/plnktype");
        definitions.setAttribute("xmlns:soap", "http://schemas.xmlsoap.org/wsdl/soap/");
        doc.appendChild(definitions);
    }

    private static void creatTypes(String name) {
        Element types = doc.createElement("types");

        Element xsdSchema = doc.createElement("xsd:schema");
        xsdSchema.setAttribute("targetNamespace", "http://j2ee.netbeans.org/wsdl/" + name);

        Element xsdImport = doc.createElement("xsd:import");
        xsdImport.setAttribute("namespace", "http://xml.netbeans.org/schema/" + name);
        xsdImport.setAttribute("schemaLocation", name + ".xsd");

        xsdSchema.appendChild(xsdImport);
        types.appendChild(xsdSchema);
        definitions.appendChild(types);
    }

    private static void creatRequestMessage(String name) {
        Element msg = doc.createElement("message");
        msg.setAttribute("name", name + "OperationRequest");

        Element part = doc.createElement("part");
        part.setAttribute("name", "request");
        part.setAttribute("element", "ns:Request");

        msg.appendChild(part);
        definitions.appendChild(msg);
    }

    private static void creatResponseMessage(String name) {
        Element msg = doc.createElement("message");
        msg.setAttribute("name", name + "OperationResponse");

        Element part = doc.createElement("part");
        part.setAttribute("name", "response");
        part.setAttribute("element", "ns:Response");

        msg.appendChild(part);
        definitions.appendChild(msg);
    }

    private static void creatPortType(String name) {
        Element portType = doc.createElement("portType");
        portType.setAttribute("name", name + "PortType");

        Element operation = doc.createElement("wsdl:operation");
        operation.setAttribute("name", name + "Operation");

        Element input = doc.createElement("wsdl:input");
        input.setAttribute("name", "request");
        input.setAttribute("message", "tns:" + name + "OperationRequest");

        Element output = doc.createElement("wsdl:output");
        output.setAttribute("name", "response");
        output.setAttribute("message", "tns:" + name + "OperationResponse");

        operation.appendChild(input);
        operation.appendChild(output);

        portType.appendChild(operation);
        definitions.appendChild(portType);
    }

    private static void creatBinding(String name) {
        Element binding = doc.createElement("binding");
        binding.setAttribute("name", name + "Binding");
        binding.setAttribute("type", "tns:" + name + "PortType");

        Element soapBinding = doc.createElement("soap:binding");
        soapBinding.setAttribute("style", "document");
        soapBinding.setAttribute("transport", "http://schemas.xmlsoap.org/soap/http");
        Element operation = doc.createElement("wsdl:operation");
        operation.setAttribute("name", name + "Operation");

        Element soapBody = doc.createElement("soap:body");
        soapBody.setAttribute("use", "literal");
        Element input = doc.createElement("wsdl:input");
        input.setAttribute("name", "request");
        input.appendChild(soapBody);
        Element output = doc.createElement("wsdl:output");
        output.setAttribute("name", "response");
        Element soapBodyOutput = doc.createElement("soap:body");
        output.appendChild(soapBodyOutput);

        operation.appendChild(input);
        operation.appendChild(output);

        binding.appendChild(soapBinding);
        binding.appendChild(operation);
        definitions.appendChild(binding);
    }

    private static void creatService(String location, int port, String name) {
        Element service = doc.createElement("service");
        service.setAttribute("name", name + "Service");

        Element wsdlport = doc.createElement("wsdl:port");
        wsdlport.setAttribute("name", name + "Port");
        wsdlport.setAttribute("binding", "tns:" + name + "Binding");

        Element address = doc.createElement("soap:address");
        address.setAttribute("location", "http://localhost:${HttpDefaultPort}/" + name + "Service/" + name + "ServicePort");
        wsdlport.appendChild(address);
        service.appendChild(wsdlport);
        definitions.appendChild(service);
    }

    private static void creatPartnerLink(String name) {
        Element link = doc.createElement("plnk:partnerLinkType");
        link.setAttribute("name", name + "PartnerLink");

        Element role = doc.createElement("plnk:role");
        role.setAttribute("name", name + "PortTypeRole");
        role.setAttribute("portType", "tns:" + name + "PortType");

        link.appendChild(role);
        definitions.appendChild(link);
    }
}
