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
 * @(#)ServiceAssemblyDescriptorGenerator.java 
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
import com.sun.jbi.cam.plugins.aspects.support.model.AspectInput;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectMap;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectOutput;
import com.sun.jbi.cam.plugins.aspects.support.model.FacadeConfiguration;

/**
 * @author graj
 *
 */
public class ServiceAssemblyDescriptorGenerator implements Serializable {
	private static final long serialVersionUID = 1L;
	static final String ASPECT_SERVICEENGINE_NAME = "sun-aspect-engine";
	static final String HTTP_BINDINGCOMPONENT_NAME = "sun-http-binding"; 

	static final String FILE_NAME_KEY = "jbi.xml";

	/**
	 * 
	 */
	public ServiceAssemblyDescriptorGenerator() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * 
	 * @param serviceAssemblyName
	 * @param serviceAssemblyDescription
	 * @param aspectMap
	 * @param facadeConfiguration
	 * @return
	 * @throws ParserConfigurationException
	 * @throws TransformerException
	 */
	public String serialize(String serviceAssemblyName,
			String serviceAssemblyDescription,
			AspectMap aspectMap,
			FacadeConfiguration facadeConfiguration)
			throws ParserConfigurationException, TransformerException {
		Map<String /* prefix */, String /* namespace */> prefixToNamespaceMap = null;
		Map<String /* namespace */, String /* prefix */> namespaceMapToPrefixMap = null;
		int namespaceIndex = 0;
		String namespaceURI = null;
		Provides provides = null;
		Consumes consumes = null;
		prefixToNamespaceMap = new HashMap<String, String>();
		namespaceMapToPrefixMap = new HashMap<String, String>();
		List<Provides> providesList = new ArrayList<Provides>();
		List<Consumes> consumesList = new ArrayList<Consumes>();
		
		QName facadeServiceQName = facadeConfiguration.getFacadeServiceQName(); 
		String facadePortName = facadeConfiguration.getFacadePortName();
		QName portTypeQName = facadeConfiguration.getPortTypeQName();
		
		List<Aspect> aspectList = aspectMap.getAspectList();
		for (Aspect aspect : aspectList) {
			AspectInput input = aspect.getInput();
			namespaceURI = input.getPartnerLinkQName().getNamespaceURI();
			if ((namespaceURI != null)
					&& (namespaceURI.trim().equals("") == false)
					&& (namespaceMapToPrefixMap.containsKey(namespaceURI) == false)) {
				namespaceMapToPrefixMap.put(namespaceURI, "ns" + namespaceIndex);
				prefixToNamespaceMap.put("ns" + namespaceIndex, namespaceURI);
				namespaceIndex++;
			}
			provides = new Provides(input.getPortTypeQName(), input
					.getPartnerLinkQName(), input.getRoleName());
                        if(providesList.contains(provides) == false) {
                            providesList.add(provides);
                        }

			List<AspectOutput> outputList = aspect.getOutputList();
			for (AspectOutput output : outputList) {
				namespaceURI = output.getServiceQName().getNamespaceURI();
				if ((namespaceURI != null)
						&& (namespaceURI.trim().equals("") == false)
						&& (namespaceMapToPrefixMap.containsKey(namespaceURI) == false)) {
					namespaceMapToPrefixMap.put(namespaceURI, "ns"
							+ namespaceIndex);
					prefixToNamespaceMap.put("ns" + namespaceIndex, namespaceURI);
					namespaceIndex++;
				}
				consumes = new Consumes(output.getPortTypeQName(), output
						.getServiceQName(), output.getPortName());
                                if(consumesList.contains(consumes) == false) {
                                    consumesList.add(consumes);
                                }
			}
		}
		namespaceURI = facadeServiceQName.getNamespaceURI();
		if ((namespaceURI != null)
				&& (namespaceURI.trim().equals("") == false)
				&& (namespaceMapToPrefixMap.containsKey(namespaceURI) == false)) {
			namespaceMapToPrefixMap.put(namespaceURI, "ns"
					+ namespaceIndex);
			prefixToNamespaceMap.put("ns" + namespaceIndex, namespaceURI);
			namespaceIndex++;
		}
		namespaceURI = portTypeQName.getNamespaceURI();
		if ((namespaceURI != null)
				&& (namespaceURI.trim().equals("") == false)
				&& (namespaceMapToPrefixMap.containsKey(namespaceURI) == false)) {
			namespaceMapToPrefixMap.put(namespaceURI, "ns"
					+ namespaceIndex);
			prefixToNamespaceMap.put("ns" + namespaceIndex, namespaceURI);
			namespaceIndex++;
		}

		// Create jbi.xml
		StringBuffer buffer = new StringBuffer();
		
		buffer.append("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n");
		buffer.append("<!--start of generated code -->\n");
		buffer.append("<jbi version=\"1.0\"\n");
		buffer.append("        xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
		buffer.append("        xmlns=\"http://java.sun.com/xml/ns/jbi\"\n");
		buffer.append("        xsi:schemaLocation=\"http://java.sun.com/xml/ns/jbi jbi.xsd\"\n");
		for (int index = 0, namespaceMapToPrefixMapSize = namespaceMapToPrefixMap
				.size(); index < namespaceMapToPrefixMapSize; index++) {
			// String ns = "ns" + (index + 1);
			String ns = "ns" + index;
			buffer.append("        xmlns:" + ns + "=\""
					+ prefixToNamespaceMap.get(ns) + "\"");
			if (index < namespaceMapToPrefixMapSize - 1) {
				buffer.append("\n");
			}
		}
		buffer.append(">\n");
		buffer.append("    <service-assembly>\n");
		buffer.append("        <identification>\n");
		buffer.append("            <name>"+serviceAssemblyName+"</name>\n");
		buffer.append("            <description>"+serviceAssemblyDescription+"</description>\n");
		buffer.append("        </identification>\n");
		buffer.append("        <service-unit>\n");
		buffer.append("            <identification>\n");
		buffer.append("                <name>"+serviceAssemblyName+"-AspectPolicyUnit</name>\n");
		buffer.append("                <description>Represents an Aspect Engine Policy Service Unit</description>\n");
		buffer.append("            </identification>\n");
		buffer.append("            <target>\n");
		buffer.append("                <artifacts-zip>AspectPolicyUnit.jar</artifacts-zip>\n");
		buffer.append("                <component-name>"+ASPECT_SERVICEENGINE_NAME+"</component-name>\n");
		buffer.append("            </target>\n");
		buffer.append("        </service-unit>\n");
		buffer.append("        \n");
		buffer.append("        <service-unit>\n");
		buffer.append("            <identification>\n");
		buffer.append("                <name>"+serviceAssemblyName+"-"+HTTP_BINDINGCOMPONENT_NAME+"</name>\n");
		buffer.append("                <description>Represents a HTTP Binding Service Unit</description>\n");
		buffer.append("            </identification>\n");
		buffer.append("            <target>\n");
		buffer.append("                <artifacts-zip>"+HTTP_BINDINGCOMPONENT_NAME+".jar</artifacts-zip>\n");
		buffer.append("                <component-name>"+HTTP_BINDINGCOMPONENT_NAME+"</component-name>\n");
		buffer.append("            </target>\n");
		buffer.append("        </service-unit>\n");
		buffer.append("        \n");
		buffer.append("        <connections>\n");
		
		String prefix = null;
		String localPart = null;
		for(Aspect aspect : aspectList) {
			AspectInput input = aspect.getInput();
			buffer.append("            <connection>\n");
			
			namespaceURI = facadeServiceQName.getNamespaceURI();
			prefix = namespaceMapToPrefixMap.get(namespaceURI);
			localPart = facadeServiceQName.getLocalPart();
			buffer.append("                <consumer endpoint-name=\""+facadePortName+"\" service-name=\""+prefix+":"+localPart+"\"/>\n");
			
			QName partnerLinkQName = input.getPartnerLinkQName();
			namespaceURI = partnerLinkQName.getNamespaceURI();
			prefix = namespaceMapToPrefixMap.get(namespaceURI);
			localPart = partnerLinkQName.getLocalPart();
			String roleName = input.getRoleName();
			buffer.append("                <provider endpoint-name=\""+roleName+"\" service-name=\""+prefix+":"+localPart+"\"/>\n");
			
			buffer.append("            </connection>\n");
		
			List<AspectOutput> outputList = aspect.getOutputList();
			for(AspectOutput output : outputList) {
				buffer.append("            <connection>\n");
				
				QName serviceQName = output.getServiceQName();
				String httpNamespaceURI = serviceQName.getNamespaceURI();
				String httpPrefix = namespaceMapToPrefixMap.get(namespaceURI);
				String httpLocalPart = serviceQName.getLocalPart();
				String httpPortName = output.getPortName();
		
				//buffer.append("                <consumer endpoint-name=\""+httpPortName+"\" service-name=\""+httpPrefix+":"+httpLocalPart+"\"/>\n");
				buffer.append("                <consumer endpoint-name=\""+roleName+"\" service-name=\""+prefix+":"+localPart+"\"/>\n");
				buffer.append("                <provider endpoint-name=\""+httpPortName+"\" service-name=\""+httpPrefix+":"+httpLocalPart+"\"/>\n");
				buffer.append("            </connection>\n");
			}
		}
		buffer.append("        </connections>\n");
		buffer.append("    </service-assembly>\n");
		buffer.append("</jbi>\n");
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
		String outputFileLocation = "/tmp/assembly_jbi.xml";
		String uri = "C:/test/aspects/demo/AspectApplication/src/aspectmap.xml";
		String serviceAssemblyName = "PolicyGroup1";
		String serviceAssemblyDescription = "Policy Group 1";
		AspectMapReader parser = null;
		AspectMap aspectMap = new AspectMap();
		ServiceAssemblyDescriptorGenerator generator = new ServiceAssemblyDescriptorGenerator();
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
			String content = generator.serialize(serviceAssemblyName, serviceAssemblyDescription,
					aspectMap, facadeConfiguration);
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
