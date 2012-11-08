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
 * @(#)BindingServiceUnitDescriptorGenerator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.model.xml;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import com.sun.jbi.cam.common.IOUtilities;
import com.sun.jbi.cam.plugins.aspects.support.model.Aspect;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectMap;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectOutput;
import com.sun.jbi.cam.plugins.aspects.support.model.FacadeConfiguration;

/**
 * @author graj
 *
 */
public class BindingServiceUnitDescriptorGenerator implements Serializable {
	private static final long serialVersionUID = 1L;


	static final String FILE_NAME_KEY = "jbi.xml";
	
	List<Provides> providesList = new ArrayList<Provides>();
	List<Consumes> consumesList = new ArrayList<Consumes>();
	
	/**
	 * 
	 */
	public BindingServiceUnitDescriptorGenerator() {
		// TODO Auto-generated constructor stub
	}
	
	
	/**
	 * @return the consumesList
	 */
	public List<Consumes> getConsumesList() {
		return consumesList;
	}


	/**
	 * @return the providesList
	 */
	public List<Provides> getProvidesList() {
		return providesList;
	}

	/**
	 * 
	 * @param aspectMap
	 * @param facadeConfiguration
	 * @return
	 * @throws ParserConfigurationException
	 * @throws TransformerException
	 */
	public String serialize(AspectMap aspectMap, FacadeConfiguration facadeConfiguration)
			throws ParserConfigurationException, TransformerException {
		Map<String /* prefix */, String /* namespace */> prefixToNamespaceMap = null;
		Map<String /* namespace */, String /* prefix */> namespaceMapToPrefixMap = null;
		prefixToNamespaceMap = new HashMap<String, String>();
		namespaceMapToPrefixMap = new HashMap<String, String>();
		int namespaceIndex = 0;
		String namespace = null;
		Provides provides = null;
		Consumes consumes = null;
		
		QName facadeServiceQName = facadeConfiguration.getFacadeServiceQName(); 
		String facadePortName = facadeConfiguration.getFacadePortName();
		QName portTypeQName = facadeConfiguration.getPortTypeQName();

		List<Aspect> aspectList = aspectMap.getAspectList();
		for (Aspect aspect : aspectList) {
			List<AspectOutput> outputList = aspect.getOutputList();
			for (AspectOutput output : outputList) {
				namespace = output.getServiceQName().getNamespaceURI();
				if ((namespace != null)
						&& (namespace.trim().equals("") == false)
						&& (namespaceMapToPrefixMap.containsKey(namespace) == false)) {
					namespaceMapToPrefixMap.put(namespace, "ns"
							+ namespaceIndex);
					prefixToNamespaceMap.put("ns" + namespaceIndex, namespace);
					namespaceIndex++;
				}
				provides = new Provides(output.getPortTypeQName(), output
						.getServiceQName(), output.getPortName());
                                if(providesList.contains(provides) == false) {
                                    providesList.add(provides);
                                }
                                consumes = new Consumes(output.getPortTypeQName(), facadeServiceQName, facadePortName);
                                if(consumesList.contains(consumes) == false) {
                                    consumesList.add(consumes);
                                }
			}
		}
		
		namespace = facadeServiceQName.getNamespaceURI();
		if ((namespace != null)
				&& (namespace.trim().equals("") == false)
				&& (namespaceMapToPrefixMap.containsKey(namespace) == false)) {
			namespaceMapToPrefixMap.put(namespace, "ns"
					+ namespaceIndex);
			prefixToNamespaceMap.put("ns" + namespaceIndex, namespace);
			namespaceIndex++;
		}

		// Generate jbi.xml
		// <?xml version='1.0'?>
		// <jbi version="1.0"
		// xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
		// xmlns="http://java.sun.com/xml/ns/jbi"
		// xsi:schemaLocation="http://java.sun.com/xml/ns/jbi jbi.xsd"
		// xmlns:ns0=${ns1} ... xmlns:nsN=${nsN} >
		// <services binding-component="false">
		// <provides interface-name=port-type service-name=partner-link
		// endpoint-name=role-name/>
		// <consumes interface-name=port-type service-name=partner-link
		// endpoint-name=role-name link-type="standard"/>
		// </services>
		// </jbi>
		StringBuffer buffer = new StringBuffer();
		buffer.append("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n");
		buffer.append("<!--start of generated code -->\n");
		buffer.append("<jbi version=\"1.0\"\n");
		buffer
				.append("        xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
		buffer.append("        xmlns=\"http://java.sun.com/xml/ns/jbi\"\n");
		buffer
				.append("        xsi:schemaLocation=\"http://java.sun.com/xml/ns/jbi jbi.xsd\"\n");
		for (int index = 0, namespaceMapToPrefixMapSize = namespaceMapToPrefixMap
				.size(); index < namespaceMapToPrefixMapSize; index++) {
			//String ns = "ns" + (index + 1);
			String ns = "ns" + index;
			buffer.append("        xmlns:" + ns + "=\""
					+ prefixToNamespaceMap.get(ns) + "\"");
			if (index < namespaceMapToPrefixMapSize - 1) {
				buffer.append("\n");
			}
		}
		buffer.append(">\n");
		buffer.append("    <services binding-component=\"true\">\n");
		String namespaceUri = null, prefix = null, localPart = null;
		// Generate all <provides> first
		for (Provides providesElement : providesList) {
			namespaceUri = providesElement.getInterfaceName().getNamespaceURI();
			prefix = namespaceMapToPrefixMap.get(namespaceUri);
			localPart = providesElement.getInterfaceName().getLocalPart();
			buffer.append("        <provides interface-name=\"" + prefix + ":"
					+ localPart);

			namespaceUri = providesElement.getServiceName().getNamespaceURI();
			prefix = namespaceMapToPrefixMap.get(namespaceUri);
			localPart = providesElement.getServiceName().getLocalPart();
			buffer.append("\" service-name=\"" + prefix + ":" + localPart);

			buffer.append("\" endpoint-name=\""
					+ providesElement.getEndpointName());
			buffer.append("\"/>\n");
		}
		// Generate all <consumes> next
		for (Consumes consumesElement : consumesList) {
			namespaceUri = consumesElement.getInterfaceName().getNamespaceURI();
			prefix = namespaceMapToPrefixMap.get(namespaceUri);
			localPart = consumesElement.getInterfaceName().getLocalPart();
                        
			buffer.append("        <consumes interface-name=\"" + prefix + ":"
					+ localPart);

			namespaceUri = consumesElement.getServiceName().getNamespaceURI();
			prefix = namespaceMapToPrefixMap.get(namespaceUri);
			localPart = consumesElement.getServiceName().getLocalPart();
			buffer.append("\" service-name=\"" + prefix + ":" + localPart);

			buffer.append("\" endpoint-name=\""
					+ consumesElement.getEndpointName());

			buffer.append("\"/>\n");
		}
		buffer.append("    </services>\n");
		buffer.append(" </jbi>\n");
		buffer.append("<!--end of generated code -->\n");

		return buffer.toString();
	}

	/**
	 * 
	 * @param content
	 * @param fileLocation
	 * @throws FileNotFoundException
	 * @throws UnsupportedEncodingException
	 * @throws IOException
	 */
	public void writeToFile(String content, String fileLocation)
			throws FileNotFoundException, UnsupportedEncodingException,
			IOException {
		OutputStream fileOutputStream = null;

		try {
			fileOutputStream = new FileOutputStream(fileLocation);
			IOUtilities.copy(content.getBytes("UTF-8"), fileOutputStream);
		} finally {
			if (fileOutputStream != null) {
				fileOutputStream.close();
			}
		}
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
	 * @param args
	 */
	public static void main(String[] args) {
		String outputFileLocation = "/tmp/http_jbi.xml";
		String uri = "C:/test/aspects/demo/AspectApplication/src/aspectmap.xml";
		AspectMapReader parser = null;
		AspectMap aspectMap = new AspectMap();
		BindingServiceUnitDescriptorGenerator generator = new BindingServiceUnitDescriptorGenerator();
		QName facadeServiceQName = new QName("http://xml.sun.jbi.aspects.facade/wsdl/FacadeSynchronousSample","service1"); 
		String facadePortName = "port1"; 
		QName portTypeQName = new QName("http://xml.sun.jbi.aspects.facade/wsdl/FacadeSynchronousSample","portType1");
		FacadeConfiguration facadeConfiguration = new FacadeConfiguration();
		facadeConfiguration.setFacadePortName(facadePortName);
		facadeConfiguration.setFacadeServiceQName(facadeServiceQName);
		facadeConfiguration.setPortTypeQName(portTypeQName);
		try {
			parser = AspectMapReader.parseFromFile(uri);
			aspectMap = parser.getAspectMap();
			String content = generator.serialize(aspectMap, facadeConfiguration);
			System.out.println(content);
			generator.writeToFile(content, outputFileLocation);
		} catch (MalformedURLException e) {
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
