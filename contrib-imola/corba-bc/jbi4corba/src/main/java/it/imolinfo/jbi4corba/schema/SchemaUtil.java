/****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.schema;

import it.imolinfo.jbi4corba.utils.HelperFileUtil;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import javax.wsdl.Definition;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.schema.Schema;
import javax.wsdl.extensions.schema.SchemaImport;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.cxf.service.model.MessageInfo;
import org.apache.cxf.service.model.MessagePartInfo;
import org.apache.cxf.service.model.OperationInfo;
import org.apache.cxf.service.model.ServiceInfo;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.extensions.schema.SchemaConstants;
import com.ibm.wsdl.extensions.schema.SchemaDeserializer;
import com.ibm.wsdl.extensions.schema.SchemaImpl;
import com.ibm.wsdl.extensions.schema.SchemaSerializer;

/**
 * This is an Utility class that contains the utility for the management
 * Xmlschema.
 * 
 * @author <a href="mailto:gvaleri@imolinfo.it">Giancarlo Valeri</a>
 */
@SuppressWarnings("unchecked")
public class SchemaUtil {

    // the logger
    @SuppressWarnings("unused")
    private static java.util.logging.Logger LOG = Logger
            .getLogger(SchemaUtil.class.getName());

    /**
     * The namespace prefix for the schema element
     */
    private static final String prefix = "xsd";

    /**
     * The QName of the schema
     */
    public static final QName Q_ELEMENT_SCHEMA = new QName(
            SchemaConstants.ELEM_SCHEMA);

    /**
     * Generates the org.w3c.dom.Element schema from list of SchemaImport
     * 
     * @param imports
     *            list of import tag
     * @return element schema generated
     */
    public final static Element generateElementSchemaFromSchemaImports(
            List<SchemaImport> imports) throws ParserConfigurationException {

        Element sc = null;

        DocumentBuilderFactory fact = DocumentBuilderFactory.newInstance();
        DocumentBuilder bd = fact.newDocumentBuilder();
        Document doc = bd.newDocument();

        // create tag schema
        sc = doc.createElement(SchemaUtil.prefix + ":"
                + SchemaConstants.ELEM_SCHEMA);
        sc.setAttribute(Constants.ATTR_XMLNS + ":" + SchemaUtil.prefix,
                SchemaConstants.NS_URI_XSD_2001);

        // add import tag
        for (int i = 0; i < imports.size(); i++) {
            Element imp = (Element) doc.createElement(SchemaUtil.prefix + ":"
                    + Constants.ELEM_IMPORT);
            imp.setAttribute(Constants.ATTR_NAMESPACE, imports.get(i)
                    .getNamespaceURI());
            imp.setAttribute(SchemaConstants.ATTR_SCHEMA_LOCATION, imports.get(
                    i).getSchemaLocationURI());
            sc.appendChild(imp);
        }
        return sc;
    }

    /**
     * Add the attribute schemaLocation in the import tag if need
     * 
     * @param element
     *            element by write
     * @param service
     *            name service
     */
    private static void addschemaLocationURI(Element element, String service) {

        String schemaPrefix = element.getPrefix();
        NodeList nodes = element.getElementsByTagName(schemaPrefix + ":"
                + Constants.ELEM_IMPORT);
        for (int i = 0; i < nodes.getLength(); i++) {
            Element el = (Element) nodes.item(i);
            String namespace = el.getAttribute(Constants.ATTR_NAMESPACE);
            String schemaLocation = el.getAttribute(Constants.ATTR_NAMESPACE);
            schemaLocation = generateFileNameXSD(schemaLocation, service);
            if (namespace.equals("http://www.w3.org/2005/08/addressing")) {
                schemaLocation = "ws-addr.xsd";
            }

            el.setAttribute(SchemaConstants.ATTR_SCHEMA_LOCATION,
                    schemaLocation);
        }

    }

    /**
     * Creates XMLSchema from an list of schemas and writes the file
     * 
     * @param schemas
     *            list schema
     * @param path
     *            path file
     * @param service
     *            name service
     * @throws java.io.IOException
     */

    public static List<String> createXSD(List<Schema> schemas, String path,
            String service) throws IOException, TransformerException {
        String xsdFileName = null;
        String xsdPathName = null;
        File xsd = null;

        Element el = null;
        String targetNamespace = null;
        FileWriter fw = null;
        List<String>  xsdFileNames = new ArrayList<String>();

        if (schemas != null && !schemas.isEmpty()) {

            for (int i = 0; i < schemas.size(); i++) {

                el = schemas.get(i).getElement();

                targetNamespace = el
                        .getAttribute(Constants.ATTR_TARGET_NAMESPACE);
                Map map = schemas.get(i).getImports();
                // add import if there is needed
                if (map != null && !map.isEmpty()) {
                    addschemaLocationURI(el, service);
                }
                xsdFileName = generateFileNameXSD(targetNamespace, service);
                // xsdFileName = service+".xsd";
                xsdPathName = path + File.separatorChar + xsdFileName;
                xsd = new File(xsdPathName);
                // Collect the file names created
                if (!xsdFileNames.contains(xsdPathName)) {
                	xsdFileNames.add(xsdPathName);
                }
                // change the boolean append from true to false for the xsd
                // generation

                fw = new FileWriter(xsd, false);
                // writes schema in the file
                writeElement(el, fw);

            }
            
        }
        return xsdFileNames;
    }

    /**
     * Generates name's file of the file XSD
     * 
     * @param targetNamespace
     *            namespace
     * @param service
     *            service name
     * @return
     */
    public static String generateFileNameXSD(String targetNamespace,
            String service) {
        String f = null;

        int length = targetNamespace.length();
        int lastSlash = targetNamespace.lastIndexOf('/');

        if (lastSlash == length - 1) {
            targetNamespace = targetNamespace.substring(0, lastSlash);
        }
        int lastSeparator = Math.max(targetNamespace.lastIndexOf('/'),
                targetNamespace.lastIndexOf(':'));

        lastSeparator = Math.max(lastSeparator, targetNamespace
                .lastIndexOf('\\'));

        targetNamespace = targetNamespace.substring(lastSeparator + 1);

        f = service + "_" + targetNamespace + ".xsd";

        return f;
    }

    /**
     * Writes the element in the FileWriter
     * 
     * @param el
     *            element by write
     * @param fw
     *            filewriter wrote
     * @throws java.io.IOException
     */
    public static void writeElement(Element el, FileWriter fw)
            throws IOException, TransformerException {

        try {
            TransformerFactory factory = TransformerFactory.newInstance();
            Transformer transformer = factory.newTransformer();
            DOMSource domSource = new DOMSource(el);
            StreamResult streamResult = new StreamResult(fw);
            transformer.transform(domSource, streamResult);
        } finally {
            if (fw != null) {
                fw.close();
            }
        }
    }

    /**
     * Utility method that registers the extensibility elements for WSDL4J.
     * 
     * @param registry
     *            The ExtensionRegistry where the partner link extension is
     *            registered.
     */
    public static void registerSchema(ExtensionRegistry registry) {
        // schema
        registry.mapExtensionTypes(javax.wsdl.Types.class, Q_ELEMENT_SCHEMA,
                SchemaImpl.class);

        registry.registerDeserializer(javax.wsdl.Types.class, Q_ELEMENT_SCHEMA,
                new SchemaDeserializer());

        registry.registerSerializer(javax.wsdl.Types.class, Q_ELEMENT_SCHEMA,
                new SchemaSerializer());
    }

    /**
     * Returns the Schemas tha contains wrapper elements in a Wrapped-style wsdl
     * 
     * @param serviceInfo
     * @param wsdlDefinition
     * @return
     */
    public static List<Schema> getSchemaWrapperList(
            final ServiceInfo serviceInfo, final Definition wsdlDefinition) {

        List<QName> wrapperNames = new ArrayList<QName>();

        // Gets the wrapperQNames
        for (OperationInfo opInfo : serviceInfo.getInterface().getOperations()) {

            if (opInfo.isUnwrappedCapable()) {
                MessagePartInfo inf = opInfo.getInput().getMessageParts()
                        .get(0);
                if (inf.getTypeClass() != null) {
                    // MessageInfo messageInfo =
                    // opInfo.getUnwrappedOperation().getInput();
                    wrapperNames.add(inf.getElementQName());
                }
                MessageInfo messageInfo = opInfo.getUnwrappedOperation()
                        .getOutput();
                if (messageInfo != null) {
                    inf = opInfo.getOutput().getMessageParts().get(0);
                    if (inf.getTypeClass() != null) {
                        wrapperNames.add(inf.getElementQName());
                    }
                }
            }
        }

        // The wrapper schema list. Contains the schemas the includes at least a
        // wrapper.
        List<Schema> schemaWrappers = new ArrayList<Schema>();
System.out.println( wsdlDefinition.getTypes());
System.out.println( wsdlDefinition.getTypes().getExtensibilityElements());
        for (int i = 0; i < wsdlDefinition.getTypes()
                .getExtensibilityElements().size(); i++) {
            Schema xmlschema = (Schema) wsdlDefinition.getTypes()
                    .getExtensibilityElements().get(i);
            boolean schemaIsWrapper = false;
            for (int j = 0; j < wrapperNames.size() && !schemaIsWrapper; j++) {
                NodeList elements = xmlschema.getElement().getChildNodes();
                QName wrapperName = wrapperNames.get(j);
                String schemaTargetNameSpace = xmlschema.getElement()
                        .getAttribute("targetNamespace");
                if ((elements != null) && (elements.getLength() != 0)) {
                    for (int k = 0; k < elements.getLength(); k++) {
                        Node element = elements.item(k); // 
                        if ((element.getAttributes() != null)
                                && (element.getAttributes()
                                        .getNamedItem("name") != null)) {
                            String elementName = element.getAttributes()
                                    .getNamedItem("name").getNodeValue();
                            if ((elementName != null)
                                    && (elementName.equals(wrapperName
                                            .getLocalPart()))
                                    && (schemaTargetNameSpace != null)
                                    && (schemaTargetNameSpace
                                            .equals(wrapperName
                                                    .getNamespaceURI()))) {
                                // If one element match, the schema contains a
                                // wrapper
                                // We check if the schema has been already
                                // added, to avoid duplicates.
                                if (!schemaIsWrapper) {
                                    schemaWrappers.add(xmlschema);
                                    schemaIsWrapper = true;
                                }
                            }
                        }
                    }
                }
            }
        }
        return schemaWrappers;
    }

    /**
     *This method create a ws-addr schema in the target Folder
     **/
    public static void createW3CSchema(String path) throws IOException,
            URISyntaxException {

        File ws = new File(path + File.separator + "ws-addr.xsd");
       
        URL schemaUrl =  SchemaUtil.class.getResource("/xsdSchema/ws-addr.xsd");

        // Copy the schema from resources
        HelperFileUtil.copyFile(schemaUrl, ws);
    }

}
