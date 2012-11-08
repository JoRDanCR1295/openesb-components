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
 * @(#)CatalogWriter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 *
 */
package com.sun.jbi.cam.plugins.aspects.support.model.catalog.xml;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Serializable;
import java.io.StringWriter;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.List;

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

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

import com.sun.jbi.cam.plugins.aspects.common.XmlConstants;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.Catalog;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.WSDLService;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.WebServices;

/**
 * @author graj
 *
 */
public class CatalogWriter implements Serializable {
    private static final long serialVersionUID = 1L;

    public static final String FILE_NAME_KEY = "catalog.xml";

    /**
     *
     */
    public CatalogWriter() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @param document
     * @return
     * @throws TransformerException
     */
    public String writeToString(Document document) throws TransformerException {
        // Use a Transformer for aspectOutput
        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer transformer = tFactory.newTransformer();
        DOMSource source = new DOMSource(document);
        StringWriter writer = new StringWriter();
        StreamResult result = new StreamResult(writer);

        transformer.setOutputProperty(OutputKeys.METHOD, "xml");
        transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
        transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, "text/xml");
        transformer.setOutputProperty(OutputKeys.STANDALONE, "yes");

        // indent the aspectOutput to make it more legible...
        transformer.setOutputProperty(
                "{http://xml.apache.org/xslt}indent-amount", "4");
        transformer.setOutputProperty(OutputKeys.INDENT, "yes");
        transformer.transform(source, result);

        return result.getWriter().toString();
    }

    /**
     *
     * @param document
     * @param directoryPath
     * @throws TransformerConfigurationException
     * @throws TransformerException
     * @throws Exception
     */
    public void writeToFile(Document document, String directoryPath)
            throws TransformerConfigurationException, TransformerException,
            Exception {
        File file = new File(directoryPath);
        if ((file.isDirectory() == false) || (file.exists() == false)) {
            throw new Exception("Directory Path: " + directoryPath
                    + " is invalid.");
        }
        String fileLocation = file.getAbsolutePath() + File.separator
                + FILE_NAME_KEY;
        System.out.println("Writing out to file: " + fileLocation);
        File outputFile = new File(fileLocation);
        // Use a Transformer for aspectOutput
        TransformerFactory tFactory = TransformerFactory.newInstance();
        Transformer transformer = tFactory.newTransformer();
        DOMSource source = new DOMSource(document);
        StreamResult result = new StreamResult(outputFile);

        // indent the aspectOutput to make it more legible...
        transformer.setOutputProperty(OutputKeys.METHOD, "xml");
        transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
        transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, "text/xml");
        transformer.setOutputProperty(OutputKeys.STANDALONE, "yes");
        transformer.setOutputProperty(
                "{http://xml.apache.org/xslt}indent-amount", "4");
        transformer.setOutputProperty(OutputKeys.INDENT, "yes");

        transformer.transform(source, result);
        System.out.println("Created " + FILE_NAME_KEY + " at: " + fileLocation);

    }

    /**
     * Change the contents of text file in its entirety, overwriting any
     * existing text. This style of implementation throws all exceptions to the
     * caller.
     *
     * @param aFile
     *            is an existing file which can be written to.
     * @throws IllegalArgumentException
     *             if param does not comply.
     * @throws FileNotFoundException
     *             if the file does not exist.
     * @throws IOException
     *             if problem encountered during write.
     */
    public void setContents(File aFile, String aContents)
            throws FileNotFoundException, IOException {
        if (aFile == null) {
            throw new IllegalArgumentException("File should not be null.");
        }
        if (!aFile.exists()) {
            aFile.createNewFile();
        }
        if (!aFile.isFile()) {
            throw new IllegalArgumentException("Should not be a directory: "
                    + aFile);
        }
        if (!aFile.canWrite()) {
            throw new IllegalArgumentException("File cannot be written: "
                    + aFile);
        }

        // declared here only to make visible to finally clause; generic
        // reference
        Writer output = null;
        try {
            // use buffering
            // FileWriter always assumes default encoding is OK!
            output = new BufferedWriter(new FileWriter(aFile));
            output.write(aContents);
        } finally {
            // flush and close both "aspectOutput" and its underlying FileWriter
            if (output != null) {
                output.close();
            }
        }
    }

    /**
     *
     * @param catalog
     * @return
     * @throws ParserConfigurationException
     * @throws TransformerException
     */
    public String serialize(Catalog catalog)
            throws ParserConfigurationException, TransformerException {
        Document document = null;
        if (catalog != null) {
            DocumentBuilderFactory factory = DocumentBuilderFactory
                    .newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            document = builder.newDocument(); // Create from whole cloth

            // ////////////////////////////////
            // <catalog>
            Element root = (Element) document
                    .createElement(XmlConstants.CATALOG_CATALOG_KEY);
            List<WebServices> servicesList = catalog.getWebServicesList();
            if (servicesList.size() > 0) {
                for (WebServices webServices : servicesList) {
                    // ////////////////////////////////
                    // <webServices>
                    Element servicesElementChild = createServicesElement(document,
                            webServices);
                    // </webServices>
                    root.appendChild(servicesElementChild);
                    // ////////////////////////////////
                }
            }
            // </catalog>
            document.appendChild(root);
            // ////////////////////////////////

        }
        return this.writeToString(document);
    }

    /**
     *
     * @param document
     * @param webServices
     * @return
     */
    Element createServicesElement(Document document, WebServices webServices) {
        Element servicesElement = null;
        if ((document != null) && (webServices != null)) {
            // <webServices>
            servicesElement = document.createElement(XmlConstants.CATALOG_SERVICES_KEY);

            // baseLocation = System.getProperty("JBICOMPS_HOME") + "/camplugins/aspects/build/web/workspace"
            servicesElement.setAttribute(XmlConstants.CATALOG_BASELOCATION_KEY, webServices.getBaseLocation().getAbsolutePath());

            List<WSDLService> serviceList = webServices.getWsdlServiceList();
            for (WSDLService wsdlService : serviceList) {
                // ////////////////////////////////
                // <wsdlService>
                Element serviceElementChild = createServiceElement(document,
                        wsdlService);
                // </aspectOutput>
                if (serviceElementChild != null) {
                    servicesElement.appendChild(serviceElementChild);
                }
                // ////////////////////////////////
            }
        }
        return servicesElement;
    }

    /**
     *
     * @param document
     * @param aspectInput
     * @return
     */
    Element createServiceElement(Document document, WSDLService wsdlService) {
        Element serviceElement = null;
        if ((document != null) && (wsdlService != null)) {
            // <service>
            serviceElement = document.createElement(XmlConstants.CATALOG_SERVICE_KEY);
            if (wsdlService.getFolder() != null) {
                // folderName="01000000-4D391F5C110100-8199A797-01"
                serviceElement.setAttribute(XmlConstants.CATALOG_FOLDERNAME_KEY, wsdlService.getFolder().getAbsolutePath());
            }
            if (wsdlService.getLocationURI() != null) {
                // locationURI="http://localhost:8080/service1"
                serviceElement.setAttribute(XmlConstants.CATALOG_LOCATIONURI_KEY, wsdlService.getLocationURI());
            }
            if (wsdlService.getName() != null) {
                // name="{tns1}service1"
                serviceElement.setAttribute(XmlConstants.CATALOG_NAME_KEY, wsdlService.getName().toString());
            }
            if (wsdlService.getServiceGroupName() != null) {
                // serviceGroupName="serviceGroupName"
                serviceElement.setAttribute(XmlConstants.CATALOG_SERVICEGROUPNAME_KEY, wsdlService.getServiceGroupName());
            }
            if (wsdlService.getPortName() != null) {
                // port="portName"
                serviceElement.setAttribute(XmlConstants.CATALOG_PORT_KEY, wsdlService.getPortName());
            }
            if (wsdlService.getFolder() != null) {
                // port="portName"
                serviceElement.setAttribute(XmlConstants.CATALOG_FOLDERNAME_KEY, wsdlService.getFolder().getAbsolutePath());
            }
            if (wsdlService.getPortType() != null) {
                // portType="{tns1}portType1"
                serviceElement.setAttribute(XmlConstants.CATALOG_PORTTYPE_KEY, wsdlService.getPortType().toString());
            }
            if (wsdlService.getTargetNamespace() != null) {
                // targetNamespace="tns1"
                serviceElement.setAttribute(XmlConstants.CATALOG_TARGETNAMESPACE_KEY, wsdlService.getTargetNamespace());
            }
            if (wsdlService.getServiceWSDLFile() != null) {
                // targetNamespace="tns1"
                serviceElement.setAttribute(XmlConstants.CATALOG_SERVICEWSDLFILE_KEY, wsdlService.getServiceWSDLFile().getAbsolutePath());
            }

            String buffer = "";
            List<File> fileList = wsdlService.getWsdlFiles();
            for(File file : fileList) {
                buffer += file.getAbsolutePath();
                buffer += " ";
            }
            // wsdlFiles="wsdl1 wsdl2 wsdl3 wsdl4"
            serviceElement.setAttribute(XmlConstants.CATALOG_WSDLFILES_KEY, buffer);

            buffer = "";
            fileList = wsdlService.getXsdFiles();
            for(File file : fileList) {
                buffer += file.getAbsolutePath();
                buffer += " ";
            }
            serviceElement.setAttribute(XmlConstants.CATALOG_XSDFILES_KEY, buffer);
        }
        return serviceElement;
    }




    /**
     * @param args
     */
    public static void main(String[] args) {
        String uri = "C:/test/aspects/catalog/catalog.xml";
        CatalogReader parser = null;
        String fileLocation = "C:/test/" + CatalogWriter.FILE_NAME_KEY;
        File file = new File(fileLocation);
        String xmlString = null;
        Catalog catalog = new Catalog();
        CatalogWriter serializer = new CatalogWriter();

        try {
            parser = CatalogReader.parseFromFile(uri);
            catalog = parser.getCatalog();
            xmlString = serializer.serialize(catalog);
            serializer.setContents(file, xmlString);
            System.out.println("");
            System.out.println(xmlString);
        } catch (MalformedURLException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (ParserConfigurationException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (SAXException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (URISyntaxException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (TransformerException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

}
