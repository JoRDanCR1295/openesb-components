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
 * @(#)AspectPolicyGroupWriter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.model.policygroup.xml;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Serializable;
import java.io.StringWriter;
import java.io.Writer;


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

import com.sun.jbi.cam.plugins.aspects.common.XmlConstants;
import com.sun.jbi.cam.plugins.aspects.support.model.policygroup.PolicyGroup;
import com.sun.jbi.cam.plugins.aspects.support.model.policygroup.PolicyGroupCollection;

/**
 * @author graj
 * 
 */
public class AspectPolicyGroupWriter implements Serializable {
	private static final long serialVersionUID = 1L;

	public static final String FILE_NAME_KEY = "policygroup.xml";

	/**
	 * 
	 */
	public AspectPolicyGroupWriter() {
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

	public String serialize(PolicyGroupCollection collection)
			throws ParserConfigurationException, TransformerException {
		Document document = null;
		if (collection != null) {
			DocumentBuilderFactory factory = DocumentBuilderFactory
					.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();
			document = builder.newDocument(); // Create from whole cloth

			// ////////////////////////////////
			// <aspectPolicyGroups>
			Element root = (Element) document
					.createElement(XmlConstants.ASPECTPOLICYGROUP_ASPECTPOLICYGROUPS_KEY);
			// /////////////////////////////////
			// <policyGroupCollection
			Element policyGroupCollectionElement = document
					.createElement(XmlConstants.ASPECTPOLICYGROUP_POLICYGROUPCOLLECTION_KEY);
			// /////////////////////////////////
			// baseLocation=System.getProperty("JBICOMPS_HOME") + "/camplugins\aspects\build\web\workspace">
			policyGroupCollectionElement.setAttribute(
					XmlConstants.ASPECTPOLICYGROUP_BASELOCATION_KEY, collection
							.getBaseLocation().getAbsolutePath());
			for (PolicyGroup group : collection.getPolicyGroupList()) {
				// /////////////////////////////////
				// <policyGroup ...>
				Element policyGroupElementChild = createPolicyGroupElement(
						document, group);
				if (policyGroupElementChild != null) {
					policyGroupCollectionElement
							.appendChild(policyGroupElementChild);
				}
				// </policyGroup>
				// /////////////////////////////////
			}
			// </policyGroupCollection>
			if (policyGroupCollectionElement != null) {
				root.appendChild(policyGroupCollectionElement);
			}
			// /////////////////////////////////
			// </aspectPolicyGroups>
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
	Element createPolicyGroupElement(Document document, PolicyGroup group) {
		Element policyGroupElement = null;
		if ((document != null) && (group != null)) {
			// <policyGroup>
			policyGroupElement = document
					.createElement(XmlConstants.ASPECTPOLICYGROUP_POLICYGROUP_KEY);
			// folderName=System.getProperty("JBICOMPS_HOME") + "/camplugins\aspects\build\web\workspace\aspectpolicygroups\PolicyGroup1"
			policyGroupElement.setAttribute(
					XmlConstants.ASPECTPOLICYGROUP_FOLDERNAME_KEY, group
							.getBaseLocation().getAbsolutePath());
			// name="PolicyGroup1"
			policyGroupElement.setAttribute(
					XmlConstants.ASPECTPOLICYGROUP_NAME_KEY, group.getName());
			// policyGroupName="Policy Group 1"
			policyGroupElement.setAttribute(
					XmlConstants.ASPECTPOLICYGROUP_POLICYGROUPNAME_KEY, group
							.getPolicyGroupName());
			// inputString="http://terraserver-usa.com/terraserver/;{http://terraserver-usa.com/terraserver/}TerraService;TerraServiceSoap;{http://terraserver-usa.com/terraserver/}TerraServiceSoap;http://terraservice.net/TerraService.asmx?WSDL;XSDurl_0,XSDurl_1,XSDurl_2,XSDurl_3,|"
			policyGroupElement.setAttribute(
					XmlConstants.ASPECTPOLICYGROUP_INPUTSTRING_KEY, group
							.getInputString());
			// facadeString="serviceName-sn;portName-pn;http://www.soap.com;targetNameSpace-tns"
			policyGroupElement.setAttribute(
					XmlConstants.ASPECTPOLICYGROUP_FACADESTRING_KEY, group
							.getFacadeString());
			// aspectString="export_asp5_mta_2;file;;;|export_asp4_la_3;WARNING;WEEKLY;/tmp/loggingse.log;|export_asp0_ca_4;GenericCache;95;;|"
			policyGroupElement.setAttribute(
					XmlConstants.ASPECTPOLICYGROUP_ASPECTSTRING_KEY, group
							.getAspectString());

		}
		return policyGroupElement;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
