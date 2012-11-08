/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.configuration;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.exception.EJBWSDLGenerationException;
import it.imolinfo.jbi4ejb.jbi.Messages;
import it.imolinfo.jbi4ejb.webservice.generator.JarUtil;
import it.imolinfo.jbi4ejb.webservice.generator.Util;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.CharacterData;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * The Class InterfaceExtractorUtil.
 */
public final class InterfaceExtractorUtil {
    
    /** The Constant EAR_TEMP_DIR. */
    private static final String EAR_TEMP_DIR = "EAR_";
    
    /** The Constant REMOTE. */
    private static final String REMOTE = "remote";
    
    /** The Constant STATELESS. */
    private static final String STATELESS = "Stateless";
    
    /** The Constant SESSION_TYPE. */
    private static final String SESSION_TYPE = "session-type";
    
    /** The Constant SESSION. */
    private static final String SESSION = "session";
    
    /** The Constant EJB. */
    private static final String EJB = "ejb";
    
    /** The Constant APPLICATION_XML. */
    private static final String APPLICATION_XML = "application.xml";
    
    /** Logger. */
    private static final Logger LOG = LoggerFactory.getLogger(InterfaceExtractorUtil.class);
    
    private static final Messages MESSAGES=Messages.getMessages(InterfaceExtractorUtil.class);
    
    /**
     * Instantiates a new interface extractor util.
     */
    private InterfaceExtractorUtil(){}
    
    /**
     * Extract remote interfaces from ear.
     * 
     * Look for the <code>application.xml</code>. For each *.jar, extracts
     * the jar names andfore each jar extracts the remote interfaces.
     * 
     * @param earPath
     *            The ear path
     * 
     * @return the list< string> of the remote interfaces
     * 
     * @throws EJBWSDLGenerationException
     *             If some problem occurs in extracting the interfaces
     */
    public static List<String> extractRemoteInterfacesFromEar(String earPath) throws EJBWSDLGenerationException {
        
        // Creates the temp dir
        File tempDir;
        try {
            tempDir = File.createTempFile(EAR_TEMP_DIR, null);
        } catch (IOException e) {
        	String msg=MESSAGES.getString("EJB000001_Could_not_create_directory", new Object[]{EAR_TEMP_DIR});
            LOG.error(msg,e);
            throw new EJBWSDLGenerationException(msg,e);
        } 
        tempDir.delete();
        tempDir.mkdir();
        
        try {
            JarUtil.unjar(new File(earPath), tempDir);
        } catch (IOException e) {
        	String msg=MESSAGES.getString("EJB000002_Failure_in_opening_Jarfile", new Object[] {earPath});
            LOG.error(msg,e);
            throw new EJBWSDLGenerationException(msg,e);
        }
        File applicationXml = findSingleFile(tempDir.getAbsolutePath(), APPLICATION_XML);
        
        if (applicationXml == null) {
            String msg=MESSAGES.getString("EJB000003_No_application.xml_found_in_earPath", new Object[] {earPath});
            LOG.error(msg);
            throw new EJBWSDLGenerationException(msg);     
        }
        
        // Gets the jar list
        List<String> jarList = getEjbJarFromApplicationXML(applicationXml);

        List<String> remoteInterfaces = new ArrayList<String>();
        
        // For each jar, retrieves the remote interface
        for (String jarName:jarList) {            
            File jarFile = findSingleFile(tempDir.getAbsolutePath(), jarName);
            remoteInterfaces.addAll(extractRemoteInterfaceFromJar(jarFile.getAbsolutePath()));
        }        
        
        // Removes the temp dir
        tempDir.delete();
        
        return remoteInterfaces;
    }
    
    /**
     * Extract remote interfaces from the jar. Look for the ejb-jar.xml (ejb
     * <2.1) and the ejb3.0 annotations
     * 
     * @param jarPath
     *            The ear path
     * 
     * @return the list< string> of the remote interfaces
     * 
     * @throws EJBWSDLGenerationException
     *             If some problem occurs in extracting the interfaces
     */    
    public static List<String> extractRemoteInterfaceFromJar(String jarPath) throws EJBWSDLGenerationException {
        
        List<String> remoteInterfaces = new ArrayList<String>();
        
        // Creates the temp dir
        File tempDir;
        try {
            tempDir = File.createTempFile("JAR_", null);
        } catch (IOException e) {
        	String msg=MESSAGES.getString("EJB000001_Could_not_create_directory", new Object[]{"JAR_"});
            LOG.error(msg,e);
            throw new EJBWSDLGenerationException(msg,e);
        	
        } 
        tempDir.delete();
        tempDir.mkdir();
        
        try {
            JarUtil.unjar(new File(jarPath), tempDir);
        } catch (IOException e) {
        	String msg=MESSAGES.getString("EJB000002_Failure_in_opening_Jarfile", new Object[] {jarPath});
            LOG.error(msg,e);
            throw new EJBWSDLGenerationException(msg,e);      	
        }
        
        File ejbJarXml = findSingleFile(tempDir.getAbsolutePath(), "ejb-jar.xml");
        List<String> interfacesListFromEjbJar = new ArrayList<String>();
        if (ejbJarXml != null) {
        // Gets the jar list from the ejb-jar        
            interfacesListFromEjbJar = getInterfacesFromEjbJarXml(ejbJarXml);
        }
        remoteInterfaces.addAll(interfacesListFromEjbJar);
        
        // Gets the jar list from the annotations (planned for release 1.0)
        // List<String> interfacesListFromAnnotations = getInterfacesFromAnnotations(tempDir.getAbsolutePath());               
        // remoteInterfaces.addAll(interfacesListFromAnnotations);

        
        // TODO remove temporary directory        
        tempDir.delete();
        
        return remoteInterfaces;
    }

    
    /**
     * Finds a file in a directory. Teh file MUST be found and MUST be one.
     * 
     * @param sourceDir
     *            The dir
     * @param fileName
     *            The file name
     * 
     * @return
     *      The file found
     * 
     * @throws EJBWSDLGenerationException
     *             If >1 files are found.
     * 
     * The file
     * 
     * The file found, null if no file found.
     */
    private static File findSingleFile(String sourceDir, String fileName) throws EJBWSDLGenerationException {
        
        File applicationXml = null;
        List<File> files = Util.findFilesFromSourceDirectoryFromExactFileName(sourceDir, fileName);
        if (files.size() > 1) {
        	String msg=MESSAGES.getString("EJB000004_Found_file_size_exceeded", new Object[] {files.size()}, new Object[] {fileName});
            LOG.error(msg);
            throw new EJBWSDLGenerationException(msg); 
        } else if (files.size() != 0) {
            applicationXml = files.get(0);    
        }        
        return applicationXml;
    }
    
    /**
     * Gets the ejb remote interfaces from ejb jar xml.
     * 
     * @param ejbJarXml
     *            The ejb-jar xml path
     * 
     * @return the interfaces from ejb jar xml
     * 
     * @throws EJBWSDLGenerationException
     *             If some xml parsing problem occurs in parsing ejb-jar.xml
     */
    private static List<String>getInterfacesFromEjbJarXml(File ejbJarXml) throws EJBWSDLGenerationException {
        List<String> interfacesList = new ArrayList<String>();

        Document doc = getDocument(ejbJarXml);

        // For each "session"
        NodeList nodes = doc.getElementsByTagName(SESSION);

        for (int i = 0; i < nodes.getLength(); i++) {
            Element element = (Element) nodes.item(i);

            NodeList sessionType = element.getElementsByTagName(SESSION_TYPE);                
            Element type = (Element) sessionType.item(0);
            // If it's a stateless, adds the remote interface to the list
            if (getCharacterDataFromElement(type).equalsIgnoreCase(STATELESS)) {
                LOG.debug("Found a stateless EJB");
                NodeList remoteElement = element.getElementsByTagName(REMOTE);
                Element remote = (Element) remoteElement.item(0);
                String remoteInterfaceName = getCharacterDataFromElement(remote);
                LOG.debug("Found a stateless EJB with interface: " + remoteInterfaceName);
                interfacesList.add(remoteInterfaceName);
            }                
        }
        
        return interfacesList;
    }
    
//    /**
//     * Gets the interfaces from annotations.
//     *
//     * This method finds the @Stateless classes, then:
//     *  - If the EJB as also the @Remote annotation, ok. else
//     *  - if one of the interfaces implemented is @Remote, ok.
//     *  
//     *  TODO implements! Palnned for 1.0
//     * @param classesDir
//     *          The classes directory
//     * @return the interfaces from annotations
//     * 
//     * @throws EJBWSDLGenerationException
//     *         If some xml parsing problem occurs in
//     */
//    private static List<String>getInterfacesFromAnnotations(String classesDir) throws EJBWSDLGenerationException {
//        List<String> interfacesList = new ArrayList<String>();
//        ClassLoader classLoader = null;
//        try {
//            classLoader = Util.getURLClassLoader(classesDir);            
//        } catch (MalformedURLException e) {
//            // TODO i18n
//            String msg = e.getMessage();
//            LOG.error(msg);
//            throw new EJBWSDLGenerationException(e);
//        }
//        // Find all the classes
//        List<File> classes = Util.findFilesFromSourceDirectory(classesDir, ".class");
//        for (File classFilePath: classes) {
//           // TODO implements... for each class, inspect it
//            
//        }
//        return interfacesList;
//    }    
//    
      
    /**
 * Gets the ejb-jar list from the application.xml file.
 * 
 * @param applicationXml
 *            The application.xml ear file
 * 
 * @return
 *  The ejb-jar list
 * 
 * @throws EJBWSDLGenerationException
 *             If some xml parsing problem occurs in parsing application.xml
 * 
 * The ejb interfaces for the ear.
 */
    private static List<String> getEjbJarFromApplicationXML(File applicationXml) throws EJBWSDLGenerationException {

        List<String> jarList = new ArrayList<String>();                      

        Document doc = getDocument(applicationXml);

        NodeList nodes = doc.getElementsByTagName(EJB);

        for (int i = 0; i < nodes.getLength(); i++) {
            Element element = (Element) nodes.item(i);
            String ejbJarName = getCharacterDataFromElement(element);
            jarList.add(ejbJarName);
        }

        return jarList;
    }
    
    /**
     * Gets the element text. <el>text</el> -> returns 'text'
     * 
     * @param e
     *            The element
     * 
     * @return
     * 
     * The element text
     */
    private static String getCharacterDataFromElement(Element e) {
        Node child = e.getFirstChild();
        if (child instanceof CharacterData) {
          CharacterData cd = (CharacterData) child;
            return cd.getData();
          }
        return "";
     }

    /**
     * Gets the <code>Document</code> form a file. Disable the dtd-validation.
     * 
     * @param file
     *            The file to be aprsed
     * 
     * @return the document
     * 
     * @throws EJBWSDLGenerationException
     *             If some parsing problem occurs.
     */
    private static Document getDocument(File file) throws EJBWSDLGenerationException {
        Document doc = null;
        try {
            
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setValidating(false);
            factory.setAttribute("http://apache.org/xml/features/nonvalidating/load-external-dtd", Boolean.FALSE);                
            DocumentBuilder builder = factory.newDocumentBuilder();
            doc = builder.parse(file);
            
        }catch (ParserConfigurationException e) {
        	String msg=MESSAGES.getString("EJB000005_Error_in_getting_document", new Object[]{file});
            LOG.error(msg,e);
            throw new EJBWSDLGenerationException(msg,e);
        }   catch (IOException e) {
        	String msg=MESSAGES.getString("EJB000005_Error_in_getting_document", new Object[]{file});
            LOG.error(msg,e);
            throw new EJBWSDLGenerationException(msg,e);
        } catch (SAXException e) {
            String msg=MESSAGES.getString("EJB000005_Error_in_getting_document", new Object[]{file});
            LOG.error(msg,e);
            throw new EJBWSDLGenerationException(msg,e);
        }    
        return doc;
    }
    
    /**
     * Extract all the ear classes in a temporary directory, to generate the
     * WSDL.
     * 
     * @param earPath
     *            The ear file path
     * 
     * @return the temporary directory path
     * 
     * @throws EJBWSDLGenerationException
     *             If some problem occurs in WSDL generation
     */
    public static String extractEarClassesInTempDirectory(String earPath)  throws EJBWSDLGenerationException {
                    
            // Creates the temp dir
            File tempDir;
            try {
                tempDir = File.createTempFile(EAR_TEMP_DIR, null);
            } catch (IOException e) {
            	String msg=MESSAGES.getString("EJB000001_Could_not_create_directory", new Object[]{EAR_TEMP_DIR});
                LOG.error(msg,e);
                throw new EJBWSDLGenerationException(msg,e);
            } 
            tempDir.delete();
            tempDir.mkdir();
            
            try {
                JarUtil.unjar(new File(earPath), tempDir);
            } catch (IOException e) {
            	String msg=MESSAGES.getString("EJB000001_Could_not_create_directory", new Object[]{EAR_TEMP_DIR});
                LOG.error(msg,e);
                throw new EJBWSDLGenerationException(msg,e);
            }
            File applicationXml = findSingleFile(tempDir.getAbsolutePath(), APPLICATION_XML);
            
            if (applicationXml == null) {
            	String msg=MESSAGES.getString("EJB000003_No_application.xml_found_in_earPath", new Object[] {earPath});
                LOG.error(msg);
                throw new EJBWSDLGenerationException(msg);
            }
            
            // Gets the jar list
            List<String> jarList = getEjbJarFromApplicationXML(applicationXml);
                        
            // For each jar, retrieves the remote interface
            for (String jarName:jarList) {
                LOG.info("EJB000006_Found_ejb-jar", new Object[]{jarName});
                File jarFile = findSingleFile(tempDir.getAbsolutePath(), jarName);
            try {
                JarUtil.unjar(jarFile, tempDir);
                } catch (IOException e) {
                	String msg=MESSAGES.getString("EJB000002_Failure_in_opening_Jarfile", new Object[] {jarFile});
                    LOG.error(msg,e);
                    throw new EJBWSDLGenerationException(msg,e);
                }
            }                    
            
            return tempDir.getAbsolutePath();
        }
    

}
