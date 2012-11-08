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
 * @(#)ComponentInformation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.management;

import java.io.File;
import java.io.Serializable;
import java.util.Iterator;
import java.util.Set;

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
import org.w3c.dom.Node;

import com.sun.jbi.cam.model.management.JBIComponentStatusDocument;
import com.sun.jbi.cam.model.management.JBIComponentStatus;

/**
 * @author ylee
 * @author Graj
 *
 */
public class ComponentInformation implements Serializable {

    static Document document;

    /**
     *
     */
    public ComponentInformation() {
        super();
        // TODO Auto-generated constructor stub
    }

    public void buildDOMTree(JBIComponentStatusDocument container) throws ParserConfigurationException {
        Iterator iterator = null;
        Set set = null;

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        document = builder.newDocument();  // Create from whole cloth


        Element root = (Element) document.createElement(JBIComponentStatusDocument.COMP_INFO_LIST_NODE_NAME);
        //////////////////////////////////////////////////////////
        // <portmaps
        //////////////////////////////////////////////////////////
        document.appendChild(root);

        JBIComponentStatus jbiComponent = null;
        iterator = container.getJbiComponentStatusList().iterator();
        //////////////////////////////////////////////////////////
        //      <portmap direction="inbound"
        //               endPoint="purchaseWsdl:schedulingPort"
        //               service=" purchaseWsdl:purchaseService" />
        //////////////////////////////////////////////////////////
        while((iterator != null) && (iterator.hasNext() == true)) {
            jbiComponent = (JBIComponentStatus) iterator.next();
            if(jbiComponent != null) {
                Element componentInfoNode = (Element) document.createElement(JBIComponentStatusDocument.COMP_INFO_NODE_NAME);

//                Element componentIdNode = (Element) document.createElement(JBIComponentDocument.ID_NODE_NAME);
//                Node componentIdTextNode = (Node)document.createTextNode(jbiComponent.getComponentId());
//                componentIdNode.appendChild(componentIdTextNode);
//                componentInfoNode.appendChild(componentIdNode);

                Element componentDescriptionNode = (Element) document.createElement(JBIComponentStatusDocument.DESCRIPTION_NODE_NAME);
                Node componentDescriptionTextNode = (Node)document.createTextNode(jbiComponent.getDescription());
                componentDescriptionNode.appendChild(componentDescriptionTextNode);
                componentInfoNode.appendChild(componentDescriptionNode);

                Element componentNameNode = (Element) document.createElement(JBIComponentStatusDocument.NAME_NODE_NAME);
                Node componentNameTextNode = (Node)document.createTextNode(jbiComponent.getName());
                componentNameNode.appendChild(componentNameTextNode);
                componentInfoNode.appendChild(componentNameNode);

                Element componentStateNode = (Element) document.createElement(JBIComponentStatusDocument.STATUS_NODE_NAME);
                Node componentStateTextNode = (Node)document.createTextNode(jbiComponent.getState());
                componentStateNode.appendChild(componentStateTextNode);
                componentInfoNode.appendChild(componentStateNode);

                Element componentTypeNode = (Element) document.createElement(JBIComponentStatusDocument.TYPE_NODE_NAME);
                Node componentTypeTextNode = (Node)document.createTextNode(jbiComponent.getType());
                componentTypeNode.appendChild(componentTypeTextNode);
                componentInfoNode.appendChild(componentTypeNode);

                root.appendChild(componentInfoNode);
            }
        }

    }

    public void writeToFile(String directoryPath) throws TransformerConfigurationException, TransformerException, Exception {
        File file = new File(directoryPath);
        if((file.isDirectory() == false) || (file.exists() == false)) {
            throw new Exception("Directory Path: "+directoryPath+" is invalid.");
        }
        String fileLocation = file.getAbsolutePath()+File.separator+"ComponentInformation.xml";
        //System.out.println("Writing out to file: "+fileLocation);
        File outputFile = new File(fileLocation);
            // Use a Transformer for output
            TransformerFactory tFactory = TransformerFactory.newInstance();
            Transformer transformer = tFactory.newTransformer();
            DOMSource source = new DOMSource(ComponentInformation.document);
            StreamResult result = new StreamResult(outputFile);

            transformer.setOutputProperty(OutputKeys.METHOD, "xml");
            transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
            transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, "text/xml");
            transformer.setOutputProperty(OutputKeys.STANDALONE, "yes");

            // indent the output to make it more legible...
            transformer.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "4" );
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.transform(source, result);
            //System.out.println("Created Component Information at: "+fileLocation);

    }


    public static void main(String[] args) {
    }
}
