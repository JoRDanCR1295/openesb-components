 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.exception.WSDLGenerationException;
import it.imolinfo.jbi4corba.jbi.JbiServiceDescriptor;
import it.imolinfo.jbi4corba.jbi.Messages;
import it.imolinfo.jbi4corba.jbi.cxf.CXFUtils;
import it.imolinfo.jbi4corba.jbi.cxf.Jbi4CorbaIdlPreprocessor;
import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaAddress;
import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaBinding;
import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaExtension;
import it.imolinfo.jbi4corba.schema.DefinitionAndSchema;
import it.imolinfo.jbi4corba.schema.SchemaUtil;
import it.imolinfo.jbi4corba.utils.HelperFileUtil;
import it.imolinfo.jbi4corba.utils.HelperIDLJUtil;
import it.imolinfo.jbi4corba.utils.HelperStringUtils;
import it.imolinfo.jbi4corba.utils.IdlFileDataHolder;
import it.imolinfo.jbi4corba.webservice.descriptor.ProviderServiceDescriptor;
import it.imolinfo.jbi4corba.webservice.runtime.ProviderServiceCreator;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.wsdl.Binding;
import javax.wsdl.BindingFault;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.schema.Schema;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.xpath.XPathConstants;

import org.apache.cxf.helpers.XMLUtils;
import org.apache.cxf.helpers.XPathUtils;
import org.apache.cxf.service.model.ServiceInfo;
import org.apache.cxf.tools.corba.idlpreprocessor.DefaultIncludeResolver;
import org.apache.cxf.tools.corba.idlpreprocessor.DefineState;
import org.apache.cxf.tools.corba.idlpreprocessor.IncludeResolver;
import org.apache.cxf.wsdl.WSDLConstants;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.extensions.schema.SchemaConstants;
import it.imolinfo.jbi4corba.jbi.wsdl.Jbi4CorbaIDLEntry;
import java.util.Set;

/**
 * Class to generate WSDL file starting from an IDL file. Usually this class is
 * used by NetBeans plugin.
 * 
 * 
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 * @author <a href="mailto:lacquaviva@imolinfo.it">Luca Acquaviva</a>
 */
public final class WSDLGenerator {

	/**
	 * The WS-ADDRESSING NAMESPACE
	 **/
	private final String WSADDNS = "http://www.w3.org/2006/03/addressing/ws-addr.xsd";
	
	private final String XSDWSNS = "http://www.w3.org/2005/08/addressing";
        
        private final String PATHSEPARATOR="/";

	private List<org.apache.cxf.service.Service> services = new ArrayList<org.apache.cxf.service.Service>();
	/**
	 * The only instance created for this class, according to <i>singleton</i>
	 * pattern.
	 */
	private static final WSDLGenerator THE_INSTANCE = new WSDLGenerator();
	/**
	 * Logger.
	 */
	private static final Logger LOG = LoggerFactory
			.getLogger(WSDLGenerator.class);
	
	private static final Messages MESSAGES = Messages
			.getMessages(WSDLGenerator.class);

	/**
	 * Creates an instance of this class.
	 */
	private WSDLGenerator() {
	}

	/**
	 * Gets an instance of this class.
	 * 
	 * @return an instance of this class, ready to use.
	 */
	public static WSDLGenerator getWSDLGenerator() {
		return THE_INSTANCE;
	}

	/**
	 * Creates a WSDL file starting from an IDL file. The generated WSDL
	 * contains only one binding, related to the JBI4Corba component.
	 * 
	 * @param desc
	 *            the descriptor incapsulating all required informations to
	 *            generate a complete WSDL. Must be not <code>null</code>.
	 * @return The WSDL definition
	 * @throws ClassGenerationException
	 *             if an error occurs during java class generation.
	 * @throws IOException
	 *             if an I/O error occurs while reading from or writing to a
	 *             file.
	 * @throws WSDLException
	 *             if an error occurs manipulating the WSDL to adjust its
	 *             content. CRB-80 New Method added generateWSDLListfromIDL
	 */
	@Deprecated
	public Definition generateWSDLfromIDL(final File idlFile,
			final WSDLDescriptor desc) throws ClassGenerationException,
			IOException, WSDLException, WSDLGenerationException,
			Jbi4CorbaException {

		ArrayList<WSDLDescriptor> descList = new ArrayList<WSDLDescriptor>();
		descList.add(desc);

		return generateWSDLListfromIDL(idlFile, descList).get(0);

	}

	/**
	 * Extract all the #include information from the original idl file
	 * 
	 * Added by
	 * 
	 * @author <a href="mailto:jussi.nummelin@nsn.com">Jussi Nummelin</a>
	 * 
	 * @param original
	 *            The original IDL file
	 * @param resolvedIDL
	 *            Temporary IDL file to write to
	 * 
	 * @throws IOException
	 *             Thrown in case of problems resolving the includes.
	 */
	private Jbi4CorbaIdlPreprocessor resolveIncludes(File original)
			throws IOException, FileNotFoundException, Jbi4CorbaException {

		try {

			URL idlUrl = original.toURI().toURL();

			final File includeDir = original.getAbsoluteFile().getParentFile();

			LOG.debug("Starting to resolve includes");

			IncludeResolver includeResolver = new DefaultIncludeResolver(
					includeDir);
			DefineState defineState = new DefineState(
					new HashMap<String, String>());

			Jbi4CorbaIdlPreprocessor preprocessor = new Jbi4CorbaIdlPreprocessor(
					idlUrl, ".", includeResolver, defineState);
                        //For each url open the file and comment the line #include                 
                        
                        
                        return preprocessor;
                        
		} finally {
			LOG.debug("All includes resolved succesfully");
		}
               
	}
        
   /**
    * Change #Include status in the original idl
    * it is used for comment #include <orb.idl> and #include <ir.idl> in the 
    * classes generation phase
    * @param idlUrls the set of file url 
    * @param status the status see {@link HelperFileUtil} for details
    * 
    * */     
   private void changeNotNeededIncludeStatus(Set<URL> idlURLs,int status) throws FileNotFoundException, IOException, Jbi4CorbaException{
                        
                        for(URL url: idlURLs){
                            try {
                                HelperFileUtil.changeIDLIncludeStaus(url, status);
                            } catch (URISyntaxException ex) {
                               
                               String msg = MESSAGES
					.getString("CRB000577_Error_On_idl_preprocessing");
                               LOG.error(msg, ex);
                               throw new Jbi4CorbaException(msg);
                            }
                        }  
   }
        
              
	/**
	 * Creates a WSDL file starting from an IDL file. The generated WSDL
	 * contains only one binding, related to the JBI4Corba component.
	 * 
	 * @param desc
	 *            the descriptor incapsulating all required informations to
	 *            generate a complete WSDL. Must be not <code>null</code>.
	 * @return The List of WSDL definition
	 * @throws ClassGenerationException
	 *             if an error occurs during java class generation.
	 * @throws IOException
	 *             if an I/O error occurs while reading from or writing to a
	 *             file.
	 * @throws WSDLException
	 *             if an error occurs manipulating the WSDL to adjust its
	 *             content.
	 * 
	 *             CRB-80 Modify the endpoint name generation the portType is
	 *             the endpointname
	 * 
	 */
	public List<Definition> generateWSDLListfromIDL(final File idlFile,
			final ArrayList<WSDLDescriptor> descList)
			throws ClassGenerationException, IOException, WSDLException,
			WSDLGenerationException, Jbi4CorbaException {
                        
                services = new ArrayList<org.apache.cxf.service.Service>();        
		// Resolve all includes
		
                Set<String> includePath;
                Set<URL>    idlTodisableImportUrl;
               
		try {
                    //Idl File is the root element file
                         Jbi4CorbaIdlPreprocessor processor=resolveIncludes(idlFile);
                         includePath =processor.getIncludesPath();
                         idlTodisableImportUrl=processor.getIdlURL();
		} catch (IOException ex) {
			String msg = MESSAGES
					.getString("CRB000575_Error_Resolving_Include");
			throw new WSDLGenerationException(msg, ex);
		}
                
                
	
		// If the file no contains interfaces it throws an exception
		if (idlInterfacesCounter(idlFile) == 0) {
			String msg = MESSAGES
					.getString("CRB000576_Error_No_interface_found_in_the_idl");
			throw new Jbi4CorbaException(msg);
		}
	
		WSDLFactory factory = WSDLFactory.newInstance();
	
		WSDLReader reader = factory.newWSDLReader();
	
                changeNotNeededIncludeStatus(idlTodisableImportUrl,HelperFileUtil.DISABLE );
                try{
		List<String> stringWsdlList = createWSDLfromIDLFile(idlFile,
				descList);
                         
		LinkedList<Definition> defList = new LinkedList<Definition>();
	
		for (String stringWsdl : stringWsdlList) {
	
			// The Position of ws-addr.xsd
			// Change the xsd location only for the wsdl generation
			// *****************************************************
			// Generate the ws-address.xsd and use it
			// Generate the file in the target directory
			// *****************************************************
	
			
			if (stringWsdl.lastIndexOf(WSADDNS) != -1) {
	
				URL schemaWsAddrUrl = this.getClass().getResource("/xsdSchema/ws-addr.xsd");				

				stringWsdl = stringWsdl.replaceAll(WSADDNS, schemaWsAddrUrl.toString());
				
			}
				 
			if (LOG.isDebugEnabled()) {
				LOG.debug(stringWsdl);
			}
	
			ExtensionRegistry registry = factory
					.newPopulatedExtensionRegistry();
			
			Definition def;
			Service service;
			Port port;
			Jbi4CorbaAddress address;
			Binding binding;
	
			WSDLDescriptor desc =null;
	
			// Reads the Definition from the String (passing through the DOM
			// parser)
			Document wsdlDoc = parseWsdlDocument(stringWsdl);
			LOG.debug("CRB000573_wsdlDoc_getDocumentURI", new Object[] { wsdlDoc
					.getDocumentURI() });
	
			// if there are many interfaces in different module's the wsdl of
			// the most
			// internal interface include the wsdl of the external interface.
			// TODO bug to fix multimodule interface
			// Disable the process of imported files for fix bug
			reader.setFeature("javax.wsdl.importDocuments", false);
			
			def = reader.readWSDL(wsdlDoc.getDocumentURI(), wsdlDoc);
			
			def.setExtensionRegistry(registry);
			def.addNamespace(Jbi4CorbaExtension.DEFAULT_PREFIX,
					Jbi4CorbaExtension.NS_URI_JBI4CORBA);
	
			for (int id = 0; id < descList.size(); id++) {
	
				if (def.getQName().equals(new QName(descList.get(id).getNamespace(),descList.get(id).getEndpointName()))) {
					desc = descList.get(id);
					break;
				}
	
			}
			
			QName oldBindingName = new QName(desc.getNamespace(), desc
					.getEndpointName()
					+ "SoapBinding");
			Jbi4CorbaBinding corbaBinding;

			// Removes the old service, which has the same name as the new one
			// we're
			// going to create
			def.removeService(new QName(desc.getNamespace(), desc
					.getCorbaServiceName()));
	
			// Adds a new Service with our estension
			service = def.createService();
			service.setQName(new QName(desc.getNamespace(), desc
					.getEndpointName()));
			def.addService(service);
	
			// Adds the port
			port = def.createPort();
			port.setName(desc.getEndpointName() + "CorbaPort");
			service.addPort(port);
	
			// Adds the extended address
			address = new Jbi4CorbaAddress();
			address.setElementType(Jbi4CorbaExtension.Q_ELEM_JBI4CORBA_ADDRESS);
			address.setName(desc.getCorbaServiceName());
			address.setLocalizationType(desc.getLocalizationType());
			address.setOrbProperties(desc.getOrbProperties());
			port.addExtensibilityElement(address);
	
			// Adds the binding
			binding = def.createBinding();
			binding.setUndefined(false);
			binding.setQName(new QName(desc.getNamespace(), desc
					.getEndpointName()
					+ "CorbaBinding"));
			binding.setPortType(def.getPortType(new QName(desc.getNamespace(),
					desc.getEndpointName())));
			port.setBinding(binding);
			def.addBinding(binding);
				
			for (Object o : def.getBinding(oldBindingName)
					.getBindingOperations()) {
				BindingOperation bo = (BindingOperation) o;
				Operation op = bo.getOperation();
				BindingInput in = bo.getBindingInput();
				BindingOutput out = bo.getBindingOutput();
	
				// Clones the <wsdl:operation> element to remove inner elements
				// contained in the "wsdlsoap" namespace
				BindingOperation newBo = def.createBindingOperation();
				Operation newOp = def.createOperation();
				BindingInput newIn = def.createBindingInput();
				BindingOutput newOut = def.createBindingOutput();
	
				newBo.setName(bo.getName());
				newOp.setName(op.getName());
				newIn.setName(in.getName());
				if (out != null) {
					newOut.setName(out.getName());
					newBo.setBindingOutput(newOut);
				}
				newBo.setOperation(newOp);
				newBo.setBindingInput(newIn);
	
				for (Object obj : bo.getBindingFaults().values()) {
					BindingFault fault = (BindingFault) obj;
					BindingFault newFault = def.createBindingFault();
	
					newFault.setName(fault.getName());
					newBo.addBindingFault(newFault);
				}
	
				binding.addBindingOperation(newBo);
			}
			def.removeBinding(oldBindingName);
	
			// Adds the extended binding
			corbaBinding = new Jbi4CorbaBinding();
			corbaBinding.setElementType(Jbi4CorbaExtension.Q_ELEM_JBI4CORBA_BINDING);
                        
                        /*****************************************************************/
                        //Insert the Root idl File
                            Jbi4CorbaIDLEntry jbi4CorbaIDLEntry=new Jbi4CorbaIDLEntry();
                            jbi4CorbaIDLEntry.setIdlFilename(idlFile.getName());
                            jbi4CorbaIDLEntry.setRoot(true);
                          
                            jbi4CorbaIDLEntry.setIDL(HelperFileUtil.readFileAsString(idlFile));
                            corbaBinding.getJbi4CorbaDLEntryList().add(jbi4CorbaIDLEntry);
                        
                        /*****************************************************************/  
                        //Adding all include idl to the wsdl    
                        for(String idlToInclude : includePath ){
                            int lastSeparator=idlToInclude.lastIndexOf(PATHSEPARATOR);
                            String fileName=idlToInclude;
                            String relativePath="";
                            if(lastSeparator>0){
                                relativePath=PATHSEPARATOR+idlToInclude.substring(0,lastSeparator)+PATHSEPARATOR;   
                                fileName=idlToInclude.substring(lastSeparator+1);
                            }
                            
                            jbi4CorbaIDLEntry=new Jbi4CorbaIDLEntry();
                            jbi4CorbaIDLEntry.setIdlFilename(fileName);
                            jbi4CorbaIDLEntry.setRoot(false);
                            jbi4CorbaIDLEntry.setRelativePath(relativePath);
                            //Add IDL
                            File idlF=new File(idlFile.getParent()+File.separator+idlToInclude);
                        
                            jbi4CorbaIDLEntry.setIDL(HelperFileUtil.readFileAsString(idlF));
                            
                            corbaBinding.getJbi4CorbaDLEntryList().add(jbi4CorbaIDLEntry);
                        
                        }
			binding.addExtensibilityElement(corbaBinding);
			
                        int length = 0;
			if (def.getTypes() != null) {
				length = def.getTypes().getExtensibilityElements().size();
			}
			LOG.debug(">>>>>>>> XSDSchema's number: " + length);
	
			// get all schema in the WSDL and add the import if is needed
			for (int i = 0; i < length; i++) {
				Schema xmlschema = (Schema) def.getTypes()
						.getExtensibilityElements().get(i);
				Map attribute = getNameSpaceList(xmlschema);
				// Add the import in the wsdl
				addRequiredSchemaImports(xmlschema, attribute);
			}
	
			defList.add(def);
	
		}
	
               
		return defList;
                }finally{
                 changeNotNeededIncludeStatus(idlTodisableImportUrl,HelperFileUtil.ENABLE );      
                
                }
                
	}

	/**
	 * Added by
	 * 
	 * @author <a href="mailto:lacquaviva@imolinfo.it">Luca Acquaviva</a>
	 *         Creates a WSDL file starting from an IDL, using the default JBI
	 *         binding.
	 * 
	 * @param idl
	 *            the IDL file to read from. Must be not <code>null</code>.
	 * @param desc
	 *            the descriptor incapsulating all required informations to
	 *            generate a complete WSDL. Must be not <code>null</code>.
	 * @return the created WSDL file or <code>null</code> if temporary
	 *         directories could not be created.
	 * @throws IOException
	 *             if an I/O error occurs while reading from or writing to a
	 *             file.
	 * @throws ClassGenerationException
	 *             if an error occurs during java class generation.
	 */
	private List<String> createWSDLfromIDLFile(final File idl,
			final List<WSDLDescriptor> descList) throws IOException,
			ClassGenerationException, Jbi4CorbaException {

		List<JbiServiceDescriptor> jbiDescList = new LinkedList<JbiServiceDescriptor>();

		CodeGenerationProperties codeGenProp = new CodeGenerationProperties();
		codeGenProp.setValueTypeImplementationWithToStringAndEquals(false);

		ProviderServiceClassesGenerator generator = new ProviderServiceClassesGenerator(
				codeGenProp);

		File tempDir = createTempDir();

		if (tempDir == null) {
			return null;
		}

		ArrayList<String> cxfWsdlList = new ArrayList<String>();

		List<String> portTypes = new ArrayList<String>();

		for (WSDLDescriptor desc : descList) {
			JbiServiceDescriptor jbiDesc = new JbiServiceDescriptor();
			jbiDesc.setIdlFileNameDirectory(idl.getParentFile()
					.getAbsolutePath());
			jbiDesc.setIdlFileName(idl.getName());
			jbiDesc.setServiceNameSpace(desc.getNamespace());
			jbiDesc.setPortTypeName(new QName(desc.getEndpointName()));

			portTypes.add(desc.getEndpointName());

			jbiDescList.add(jbiDesc);

		}

		try {

			List<ClientCorbaClassesHolder> classes = generator
					.generateProviderServiceClasses(jbiDescList, tempDir
							.getAbsolutePath(), (String) null, portTypes);

			// May be one to multiple classes CRB-80
			// if the idl file contains more than one interface there are many
			// corbaClasses

			for (ClientCorbaClassesHolder corbaClasses : classes) {
				WSDLDescriptor desc = null;
				
				String 	className=corbaClasses
				.getOperationsClass().getName().substring(corbaClasses
						.getOperationsClass().getName().lastIndexOf(".")+1,corbaClasses
						.getOperationsClass().getName().length()-10);
			
				String namespace = corbaClasses.getOperationsClass().getName()
                                                .substring(0,corbaClasses.getOperationsClass().getName().length()-10);
			
				// If there is only one interfaces it takes the first descriptor
				desc = descList.get(0);
				if (classes.size() > 1) {
					desc = getWsdlDescByIntName(className,namespace, descList);
				}
				// if descriptor is not null than it generate the wsdl
				if (desc != null) {

					ProviderServiceDescriptor serviceDesc = new ProviderServiceDescriptor();
					org.apache.cxf.service.Service service;

					QName interfaceName = new QName(desc.getNamespace(), desc
							.getEndpointName());

					serviceDesc.setServiceInterface(corbaClasses
							.getOperationsClass());
					serviceDesc.setCorbaHelperClass(corbaClasses
							.getHelperClass());
					serviceDesc.setServiceName(desc.getCorbaServiceName());
					serviceDesc.setServiceNameSpace(desc.getNamespace());
					String cxfWsdl = null;
					ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

					try {

						service = new ProviderServiceCreator().createService(
								serviceDesc, interfaceName);

						CXFUtils.writeDefinitionOnOutputStream(service,
								outputStream);
						services.add(service);

					} catch (WSDLException e) {
						String msg = MESSAGES
								.getString("CRB000574_Error_in_WSDL_generation");
						LOG.error(msg, e);
						throw new ClassGenerationException(msg, e);

					} catch (Jbi4CorbaException e) {
						String msg = MESSAGES
								.getString("CRB000574_Error_in_WSDL_generation");
						LOG.error(msg, e);
						throw new ClassGenerationException(msg, e);
					}

					cxfWsdl = outputStream.toString();
					cxfWsdlList.add(cxfWsdl);
				}

			}

			return cxfWsdlList;

		} finally {
			deleteDirectory(tempDir);
		}
	}

	/**
	 * This method return the correct descriptor when there are many interfaces
	 * It return the descriptor based on the matching ofthe name of class an an
	 * Endpoint Name
	 * 
	 * @param List
	 *            <WSDLDescriptor>
	 * @param String
	 *            name the interface name
	 * @return WSDLDescriptor
	 */
	private WSDLDescriptor getWsdlDescByIntName(final String name,final String namespace,
			final List<WSDLDescriptor> descList) {

		for (WSDLDescriptor desc : descList) {
			
			if (desc.getEndpointName().equals(name) && desc.getNamespace().equals("http://"+namespace)) {
				return desc;
			}
		}
		//Added To Run OldTest
		for (WSDLDescriptor desc : descList) {
			
			if (desc.getEndpointName().equals(name)) {
				return desc;
			}
		}

		
		return null;
	}

	/**
	 * This method should be used in sequence with the method
	 * generateWSDLListfromIDL it accepts the definition generated list from
	 * method <code>generateWSDLListfromIDL</code> 1) Remove the content of tag
	 * <code>schema</code> and add the import schema file generated; 2) Create
	 * xsd schema for each xmlSchema in the WSDL. 3)
	 * 
	 * @param definition
	 *            Definition WSDlL
	 * @param service
	 *            service's name
	 * @throws java.io.IOException
	 * @throws javax.xml.parsers.ParserConfigurationException
	 * @return definition without schema anda separated schema
	 */
	public DefinitionAndSchema createRemoveXSD(List<Definition> definition)
			throws IOException, ParserConfigurationException,
			TransformerException {

		List<Schema> schemas = new ArrayList<Schema>();
		DefinitionAndSchema defandschema = new DefinitionAndSchema();
		int index = 0;
		for (Definition def : definition) {

			ServiceInfo serviceInfo = services.get(index).getServiceInfos()
					.get(0);
			List<Schema> wrapperList = SchemaUtil.getSchemaWrapperList(
					serviceInfo, def);

			int length = def.getTypes().getExtensibilityElements().size();

			LOG.debug(">>>>>>>> XSDSchema's number: " + length);

			// get all schema in the WSDL
			for (int i = 0; i < length; i++) {
				Schema xmlschema = (Schema) def.getTypes()
						.getExtensibilityElements().get(i);
				
				if (!schemas.contains(xmlschema)) {
					schemas.add(xmlschema);
					
				}
			}
                        // Remove the wrapper from the schemalist for mantain this inline in
			// the wsdl
			for (int ii = 0; ii < wrapperList.size(); ii++) {
				if (schemas.contains(wrapperList.get(ii))) {
					schemas.remove(wrapperList.get(ii));
                                                                             
				}
			}
						 
			for (int i = 0; i < schemas.size(); i++) {

				// Remove all the schema from the wsdl
				// remove tags schema from definition
				def.getTypes().getExtensibilityElements().removeAll(schemas);
				// Remove the wrapper
				def.getTypes().getExtensibilityElements()
						.removeAll(wrapperList);

				
			}
			// MARCO: If there's no schema, this logic shouldn't be executed 
			if (schemas.size() != 0) {
				Schema wrapper = null;
				if (wrapperList.size() > 0) {

					wrapper = wrapperList.get(0);

					// Change import type in wrapper

					Element element = wrapper.getElement();
					String schemaPrefix = element.getPrefix();

					NodeList nodes = element.getElementsByTagName(schemaPrefix
							+ ":" + Constants.ELEM_IMPORT);
					for (int ii = 0; ii < nodes.getLength(); ii++) {
						Element el = (Element) nodes.item(ii);
						String namespace = el
								.getAttribute(Constants.ATTR_NAMESPACE);
						String schemaLocation = SchemaUtil.generateFileNameXSD(
								namespace, "TypeDef");
						// Added for manage ENDPOINT-REFERENCE W3C
						if (namespace.equals(XSDWSNS)) {
							defandschema.setContainsW3c(true);
							schemaLocation = "ws-addr.xsd";
						}

						el.setAttribute(SchemaConstants.ATTR_SCHEMA_LOCATION,
								schemaLocation);
					}

					// Add the wrapper
					if (wrapper != null) {

						def.getTypes().getExtensibilityElements().add(wrapper);
					}
				}
			//}
			}
		
			
			index++;
		}

		defandschema.setDefinition(definition);
		defandschema.setSchemas(schemas);
		return defandschema;

	}

	/**
	 * 
	 * Returns the numbers of Interfaces from an IDL
	 * 
	 * @param idl
	 *            the IDL file to read from. Must be not <code>null</code>.
	 * @return
	 * @throws IOException
	 *             if an I/O error occurs while reading file.
	 */
	public int idlInterfacesCounter(final File idlFile) throws IOException,
			Jbi4CorbaException {
		
		return idlInterfacesNames(idlFile).size();

	}

	/**
	 * 
	 * Returns the Interface's name from an IDL
	 * 
	 * @param idl
	 *            the IDL file to read from. Must be not <code>null</code>.
	 * @return Interfaces Names
	 * @throws IOException
	 *             if an I/O error occurs while reading file.
	 */
	public List<String> idlInterfacesNames(final File idlFile)
			throws IOException, Jbi4CorbaException {

		CodeGenerationProperties codeGenProp = new CodeGenerationProperties();
		codeGenProp.setValueTypeImplementationWithToStringAndEquals(false);
		List<String> names = new ArrayList<String>();

		File tempDir = createTempDir();

		if (tempDir == null) {
			return null;
		}
                 Set<URL> idlUrl= resolveIncludes(idlFile).getIdlURL();
                //Comment not needed import
                changeNotNeededIncludeStatus(idlUrl,HelperFileUtil.DISABLE );
		HelperIDLJUtil.idlj(tempDir.getAbsolutePath(), idlFile.getParent(), idlFile
				.getCanonicalPath());
                changeNotNeededIncludeStatus(idlUrl,HelperFileUtil.ENABLE );
		List<String> javaSources = Util.findJavaSources(tempDir
				.getCanonicalPath());
		for (String canFilename : javaSources) {
			if (canFilename.endsWith("Operations.java")) {
				String name=HelperStringUtils.ExtractString(canFilename, String
						.valueOf(File.separator), "Operations.java");
				                                
                        names.add(name);
			}
		}

		deleteDirectory(tempDir);

		return names;
	}

	/**
	 * 
	 * Returns The names' and namespace's from an idl
	 * 
	 * @param idl
	 *            the IDL file to read from. Must be not <code>null</code>.
	 * @return Interfaces Names
	 * @throws IOException
	 *             if an I/O error occurs while reading file.
	 */
	public List<IdlFileDataHolder> getIdlFileData(final File idlFile)
			throws IOException, Jbi4CorbaException {

		
		CodeGenerationProperties codeGenProp = new CodeGenerationProperties();
		codeGenProp.setValueTypeImplementationWithToStringAndEquals(false);

		File tempDir = createTempDir();

		if (tempDir == null) {
			return null;
		}
                Set<URL> idlUrl= resolveIncludes(idlFile).getIdlURL();
                //Comment not needed import
                changeNotNeededIncludeStatus(idlUrl,HelperFileUtil.DISABLE );
		HelperIDLJUtil.idlj(tempDir.getAbsolutePath(), idlFile.getParent(), idlFile
				.getCanonicalPath());
                //DeComment not needed import
                changeNotNeededIncludeStatus(idlUrl,HelperFileUtil.ENABLE );
		List<IdlFileDataHolder> holderList = new ArrayList<IdlFileDataHolder>();
		IdlFileDataHolder holder;

		List<String> javaSources = Util.findJavaSources(tempDir
				.getCanonicalPath());
		for (String canFilename : javaSources) {
			if (canFilename.endsWith("Operations.java")) {				
				String name = HelperStringUtils.ExtractString(canFilename,
						String.valueOf(File.separator), "Operations.java");
				
			
				if(!javaSources.contains(canFilename.substring(0,canFilename.length()-15)+"OperationsOperations.java")){
					
					int lastIndex = canFilename.lastIndexOf(File.separator);
					int startIndex = tempDir.getCanonicalPath().length() + 1;

					String nameSpace = "default";
					if (lastIndex - startIndex > 0) {
						nameSpace = canFilename.substring(startIndex, lastIndex);
					}

					nameSpace = Util.replaceSeparatorWithDot(nameSpace);
					nameSpace = "http://" + nameSpace + "." + name;
					holder = new IdlFileDataHolder(name, nameSpace);
					holderList.add(holder);
					}
				}

		}

		deleteDirectory(tempDir);

		return holderList;
	}

	/**
	 * Creates a temporary directory.
	 * 
	 * @return the created directory or <code>null</code> if the creation has
	 *         failed even though a file has been created.
	 * @throws IOException
	 *             if a file could not be created.
	 */
	private static File createTempDir() throws IOException {
		File f = File.createTempFile("IDL2WSDLWIZARD_", null);

		/*
		 * java.io.File API can create only temporary files: delete the created
		 * file and make a directory with the same name
		 */
		if (!f.delete()) {
			return null;
		}
		if (!f.mkdir()) {
			return null;
		}
		return f;
	}

	/**
	 * Tries to delete a directory, even though it's not empty.
	 * 
	 * @param directory
	 *            the directory to delete. Must be not <code>null</code>.
	 */
	private static void deleteDirectory(final File directory) {
		File[] files = directory.listFiles();

		if (files != null) {
			for (File f : files) {
				if (f.isDirectory()) {
					deleteDirectory(f);
				} else {
					f.delete();
				}
			}
		}
		directory.delete();
	}

	/**
	 * Parses the wsdl document into a <code>Document</code>
	 * 
	 * @param wsdlDoc
	 * 
	 * @return the document
	 * 
	 * @throws Jbi4CorbaException
	 */
	private Document parseWsdlDocument(String wsdlDoc)
			throws WSDLGenerationException {

		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		DocumentBuilder mBuilder;
		Document wrappedDoc = null;

		try {
			factory.setNamespaceAware(true);
			mBuilder = factory.newDocumentBuilder();
			
			// Bug Multimodule Interface
			wrappedDoc = mBuilder.parse(new InputSource(
					new java.io.StringReader(wsdlDoc)));

		} catch (ParserConfigurationException ex) {
			Object[] args = new Object[] { ex.getMessage() };
			LOG.error("CRB000554_Error_during_wsdl_parsing", args, ex);
			throw new WSDLGenerationException(
					"CRB000554_Error_during_wsdl_parsing", args, ex);
		} catch (SAXException ex) {
			Object[] args = new Object[] { ex.getMessage() };
			LOG.error("CRB000554_Error_during_wsdl_parsing", args, ex);
			throw new WSDLGenerationException(
					"CRB000554_Error_during_wsdl_parsing", args, ex);
		} catch (IOException ex) {
			Object[] args = new Object[] { ex.getMessage() };
			LOG.error("CRB000554_Error_during_wsdl_parsing", args, ex);
			throw new WSDLGenerationException(
					"CRB000554_Error_during_wsdl_parsing", args, ex);
		}
		LOG.debug("wrappedDoc.class" + wrappedDoc.getClass());
		return wrappedDoc;
	}

	/**
	 * Add schema imports if is needed.
	 * 
	 * @param schemaElement
	 * @param attribute
	 */
	protected void addRequiredSchemaImports(Schema schema, Map attribute) {

		LOG.debug("addRequiredSchemaImports - BEGIN");
		Element schemaElement = schema.getElement();
		String schemaNamespace = schemaElement
				.getAttribute(Constants.ATTR_TARGET_NAMESPACE);

		Map<String, String> queryPrefixMap = new HashMap<String, String>();
		queryPrefixMap.put("xs", WSDLConstants.NP_SCHEMA_XSD);
		XPathUtils xpu = new XPathUtils(queryPrefixMap);

		Iterator iterator = attribute.keySet().iterator();
		do {
			String prefix = (String) iterator.next();
			String namespace = (String) attribute.get(prefix);

			if (!namespace.equals(schemaNamespace)
					&& !namespace.equals(WSDLConstants.NP_SCHEMA_XSD)
					&& !xpu.isExist(
							"xs:import[@namespace='" + namespace + "']",
							schemaElement, XPathConstants.NODE)
					&& !xpu.isExist("xsd:import[@namespace='" + namespace
							+ "']", schemaElement, XPathConstants.NODE)
					&& !namespace.equals(WSDLConstants.NS_SCHEMA_XSD)
					&& !namespace.equals(XSDWSNS)
					&& schema.getImports().get(namespace) == null

			) {

				LOG.debug("Add Import for namespace: " + namespace);
				Element importElement = XMLUtils.createElementNS(schemaElement
						.getOwnerDocument(), new QName(schemaElement
						.getPrefix()
						+ ":import"));

				importElement.setAttribute("namespace", namespace);
				schemaElement.insertBefore(importElement, schemaElement
						.getFirstChild());

				schema.getImports().put("namespace", namespace);
			}
		} while (iterator.hasNext());
		LOG.debug("addRequiredSchemaImports - END");
	}

	/**
	 * Get namespace declarated in the schema tag.
	 * 
	 * @param schema
	 * @return
	 */
	private Map getNameSpaceList(Schema schema) {

		Element el = schema.getElement();
		NamedNodeMap mapAttr = el.getAttributes();
		Map result = new HashMap();

		for (int i = 0; i < mapAttr.getLength(); i++) {

			String nsURI = mapAttr.item(i).getNamespaceURI();
			if (nsURI != null && nsURI.equals(Constants.NS_URI_XMLNS)) {
				String prefix = mapAttr.item(i).getLocalName();
				String value = mapAttr.item(i).getNodeValue();
				LOG.debug("Found namespace: " + mapAttr.item(i));

				result.put(prefix, value);
			}
		}
		// LOG.debug("Map : " + result);
		return result;
	}
}
