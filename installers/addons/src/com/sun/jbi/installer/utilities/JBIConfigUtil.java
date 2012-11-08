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
 * @(#)JBIConfigUtil.java - ver 1.1 - 01/04/2006
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.installer.utilities;

import java.io.File;
import java.io.IOException;
import java.io.FileOutputStream;
import java.io.StringReader;
import java.util.Properties;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.FactoryConfigurationError;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.Source;
import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.OutputKeys;
/*
import com.sun.appserv.management.DomainRoot;
import com.sun.appserv.management.j2ee.J2EEDomain;
import com.sun.appserv.management.client.ProxyFactory;
import com.sun.enterprise.admin.common.MBeanServerFactory;
*/
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.InputSource;
import org.xml.sax.EntityResolver;
import org.w3c.dom.Document;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;

/**
 * This class is used to parse the domain configuration file of
 * a domain and provides methods to query/update the domain config.
 */
public class JBIConfigUtil {

	public static final String EDIT_JBI_REGISTRY 	= "-EditJBIRegistry";
    public static final String BOOTSTRAP_CLSF 		= "bootstrapClassLoaderSelfFirst";
    public static final String COMPONENT_CLSF 		= "componentClassLoaderSelfFirst";
    public static final String INSTALL_ROOT 		= "install-root";
    public static final String NAME_REF 			= "name-ref";
    public static final String STATE 				= "state";
    public static final String WORKSPACE 			= "workspace";
    public static final String SHARED_LIBRARIES_NODE = "shared-libraries";
    public static final String SHARED_LIBRARY_NODE = "shared-library";
    public static final String SHARED_LIBRARY_REF_NODE = "shared-library-ref";
    public static final String COMPONENTS_NODE 		= "components";
    public static final String COMPONENT_NODE 		= "component";
    public static final String SERVERS_NODE 		= "servers";
    public static final String SERVER_NODE 			= "server";
    public static final String COMPONENT_REF_NODE 	= "component-ref";
    public static final String NAME_ATTR 			= "name";
    public static final String NAME_REF_ATTR 			= "name-ref";
    public static final String TIMESTAMP_ATTR 			= "timestamp";
    public static final String FILE_NAME_ATTR 		= "file-name";
    public static final String START_ON_DEPLOY_ATTR = "start-on-deploy";
    public static final String SYSTEM_INSTALL_ATTR = "system-install";
    public static final String SERVICE_UNITS_NODE 	= "service-units";
    public static final String SERVER_VALUE 		= "server";
    public static final String INSTANCE_VALUE 		= "instance1";
    public static final String IEP = "iep";
  /*  
    private static ProxyFactory amxProxyFactory = null;
	 private static DomainRoot domainRoot = null;
    private static J2EEDomain j2eeDomain = null;
    */        
    public static void main(String args[]) {
    	if (args[0].equals(EDIT_JBI_REGISTRY)) {
    		boolean isDAS = false;
    		if (args[4].equals("true")) {
    			isDAS = true;
    		}
    		//domainRoot, component name, jar file name, and DAS flag
			if (args[3].indexOf("lib") != -1) {
    			editJBIRegistry4Lib(args[1], args[2], args[3], isDAS); 
			} else {
    			editJBIRegistry4Comp(args[1], args[2], args[3], isDAS); 
			}

      }
    }
    
    /**
     * This method updates the JBI registry xml file
     * for the installed components.
     */
    public static boolean editJBIRegistry4Comp(String domainRoot, String compName, 
    									  String jarName, boolean isDAS)
    {

        try {
        	String jbiRegistryfile = domainRoot + File.separator +
        							"jbi" + File.separator + "config" + 
        							File.separator + "jbi-registry.xml";
            Document document = getDocument(jbiRegistryfile);
            NodeList compsList = document.getElementsByTagName(COMPONENTS_NODE);
            //as there will be only one node named components so we take the first one.
            Node comps = compsList.item(0);
            if (comps == null) {
                return false;
            }
            NodeList compList = document.getElementsByTagName(COMPONENT_NODE);
            if (!nodeExists(compList, compName, NAME_ATTR)) {
	            Element compNode = document.createElement(COMPONENT_NODE);
	            compNode.setAttribute(FILE_NAME_ATTR, jarName);
	            compNode.setAttribute(NAME_ATTR, compName);
	            compNode.setAttribute(TIMESTAMP_ATTR, "0");
	            comps.appendChild(compNode);
	        }

			  String state = "Shutdown";
			/*
			  if (compName.indexOf(IEP) != -1) {
					state = "Stopped";
			  }
			*/

            Element compRef = document.createElement(COMPONENT_REF_NODE);
            compRef.setAttribute(INSTALL_ROOT, 
            	"${com.sun.aas.instanceRoot}/jbi/components/" + compName + "/install_root");
            compRef.setAttribute(NAME_REF, compName);
            compRef.setAttribute(STATE, state);
            compRef.setAttribute(WORKSPACE, 
            	"${com.sun.aas.instanceRoot}/jbi/components/" +  compName + "/install_root/workspace");
            	
           Element serviceUnits = document.createElement(SERVICE_UNITS_NODE);
           compRef.appendChild(serviceUnits);
            	
            NodeList serversList = document.getElementsByTagName(SERVERS_NODE);
            // There is only one node called "servers"
            Node serversNode = serversList.item(0);
            if (serversNode == null) {
                return false;
            }
            
			NodeList serverList = document.getElementsByTagName(SERVER_NODE);
			for(int i = 0; i < serverList.getLength(); i++){
				Element server = (Element) serverList.item(i);
				Node sharedLibRef = findNodeToInsertBefore(server.getChildNodes(), SHARED_LIBRARY_REF_NODE);
				if (server.getAttribute(NAME_REF).equals(SERVER_VALUE) && isDAS) {
					if (!nodeExists(server.getChildNodes(), compName, NAME_REF_ATTR)) { 
                    	server.insertBefore(compRef, sharedLibRef);
                    }
	            } else if (!server.getAttribute(NAME_REF).equals(SERVER_VALUE) && !isDAS) {
                	if (!nodeExists(server.getChildNodes(), compName, NAME_REF_ATTR)) { 
                		server.insertBefore(compRef, sharedLibRef);
                	}
	            }
	        }

            TransformerFactory xformFactory = TransformerFactory.newInstance();
            Transformer transformer = xformFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty(OutputKeys.METHOD, "xml");
				/*
            transformer.setOutputProperty(OutputKeys.DOCTYPE_PUBLIC,
                    document.getDoctype().getPublicId());
            transformer.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM,
                    document.getDoctype().getSystemId());
						  */
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");

            Source input = new DOMSource(document);
            Result output = new StreamResult(new File(jbiRegistryfile));
            transformer.transform(input, output);
            return true;
        } catch (Exception ex) {
            ex.printStackTrace();
            return false;
        }
    }
    
        /**
     * This method updates the JBI registry xml file
     * for the installed components.
     */
    public static boolean editJBIRegistry4Lib(String domainRoot, String compName, 
    									  String jarName, boolean isDAS)
    {

        try {
        	String jbiRegistryfile = domainRoot + File.separator +
        							"jbi" + File.separator + "config" + 
        							File.separator + "jbi-registry.xml";
            Document document = getDocument(jbiRegistryfile);
            NodeList sLibList = document.getElementsByTagName(SHARED_LIBRARIES_NODE);
            //as there will be only one node named components so we take the first one.
            Node slibs = sLibList.item(0);
            if (slibs == null) {
                return false;
            }
            NodeList slibList = document.getElementsByTagName(SHARED_LIBRARY_NODE);
            if (!nodeExists(slibList, compName, NAME_ATTR)) {
	            Element slibNode = document.createElement(SHARED_LIBRARY_NODE);
	            slibNode.setAttribute(FILE_NAME_ATTR, jarName);
	            slibNode.setAttribute(NAME_ATTR, compName);
	            slibNode.setAttribute(TIMESTAMP_ATTR, "0");
	            slibs.appendChild(slibNode);
	        }
            
            Element slibRef = document.createElement(SHARED_LIBRARY_REF_NODE);
            slibRef.setAttribute(INSTALL_ROOT, 
            	"${com.sun.aas.instanceRoot}/jbi/shared-libraries/" + compName + "/install_root");
            slibRef.setAttribute(NAME_REF, compName);
            	
            NodeList serversList = document.getElementsByTagName(SERVERS_NODE);
            // There is only one node called "servers"
            Node serversNode = serversList.item(0);
            if (serversNode == null) {
                return false;
            }
            
			NodeList serverList = document.getElementsByTagName(SERVER_NODE);
			for(int i = 0; i < serverList.getLength(); i++){
				Element server = (Element) serverList.item(i);
				if (server.getAttribute(NAME_REF).equals(SERVER_VALUE) && isDAS) {
                	if (!nodeExists(server.getChildNodes(), compName, NAME_REF_ATTR)) { 
                		server.appendChild(slibRef);
                	}
            } else if (!server.getAttribute(NAME_REF).equals(SERVER_VALUE) && !isDAS) {
                	if (!nodeExists(server.getChildNodes(), compName, NAME_REF_ATTR)) { 
                		server.appendChild(slibRef);
                	}
            }
         }

            TransformerFactory xformFactory = TransformerFactory.newInstance();
            Transformer transformer = xformFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty(OutputKeys.METHOD, "xml");
				/*
            transformer.setOutputProperty(OutputKeys.DOCTYPE_PUBLIC,
                    document.getDoctype().getPublicId());
            transformer.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM,
                    document.getDoctype().getSystemId());
						  */
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");

            Source input = new DOMSource(document);
            Result output = new StreamResult(new File(jbiRegistryfile));
            transformer.transform(input, output);
            return true;
        } catch (Exception ex) {
            ex.printStackTrace();
            return false;
        }
    }

	/**
     * This method decides the node exists or not.
     */
    public static boolean nodeExists(NodeList nodeList, String compName, String attr) {
    	for(int i = 0; i < nodeList.getLength(); i++){
            Node node = (Node) nodeList.item(i);
            if (node.getNodeType() == Node.ELEMENT_NODE) {
            	Element e = (Element) node;
	            if (e.getAttribute(attr).equals(compName)) {
	                return true;
	            }
	        }
        }
    	return false;
	}
	
	/**
     * This method decides that all nodes will be inserted before this one.
     */
    public static Node findNodeToInsertBefore(NodeList nodeList, String
	 nodeName) {
    	for(int i = 0; i < nodeList.getLength(); i++){
            Node node = (Node) nodeList.item(i);
            if (node.getNodeType() == Node.ELEMENT_NODE) {
            	Element e = (Element) node;
	            if (e.getNodeName().equals(nodeName)) {
	                return node;
	            }
	        }
        }
    	return null;
	}

    /**
     * This method is used to parse the JBI registry file and return a
     * document
     */
    public static Document getDocument(String jbiRegistryfile) {
        DocumentBuilderFactory factory =
                DocumentBuilderFactory.newInstance();
        factory.setValidating(false);
        try {
            DocumentBuilder builder = factory.newDocumentBuilder();
            builder.setEntityResolver(new EntityResolver(){
                public InputSource resolveEntity(
                        String publicId,
                        String systemId) throws SAXException, IOException
                {
                    StringReader reader =
                        new StringReader(
                            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
                    InputSource source = new InputSource(reader);
                    source.setPublicId(publicId);
                    source.setSystemId(systemId);
                    return source;
                }
            });
            Document document = builder.parse(new File(jbiRegistryfile));
            return document;
        } catch (SAXException sxe) {
            sxe.printStackTrace();
            return null;
        } catch (ParserConfigurationException pce) {
            pce.printStackTrace();
            return null;
        } catch (IOException ioe) {
           ioe.printStackTrace();
           return null;
        }
    }
    /*
    public static J2EEDomain getJ2EEDomain() {
        return j2eeDomain == null ? getDomainRoot().getJ2EEDomain() : j2eeDomain;
    }
    
    public static DomainRoot getDomainRoot() {
        return domainRoot == null ? getAMXProxyFactory().getDomainRoot() : domainRoot;
    }
    
    public static ProxyFactory getAMXProxyFactory() {
        return amxProxyFactory == null ?
        ProxyFactory.getInstance( MBeanServerFactory.getMBeanServer()) : amxProxyFactory;
    }
    */
}
