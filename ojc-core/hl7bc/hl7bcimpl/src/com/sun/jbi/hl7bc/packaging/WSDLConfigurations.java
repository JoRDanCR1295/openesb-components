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
 * @(#)WSDLConfigurations.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.packaging;

import java.io.File;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.regex.Pattern;
import java.net.URL;

import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Input;
import javax.wsdl.Output;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.OperationType;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.xml.sax.EntityResolver;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.apache.xml.resolver.CatalogManager;
import org.w3c.dom.Document;

import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.MetaRef;
import com.sun.encoder.tools.xml.XsdLocator;
import com.sun.jbi.hl7bc.Endpoint;
import com.sun.jbi.hl7bc.EndpointImpl;
import com.sun.jbi.hl7bc.extensions.HL7Address;
import com.sun.jbi.hl7bc.extensions.HL7Binding;
import com.sun.jbi.hl7bc.extensions.HL7ExtensionRegistry;
import com.sun.jbi.hl7bc.extensions.HL7ExtPreprocessDeserializer;
import com.sun.jbi.hl7bc.extensions.HL7Operation;
import com.sun.jbi.hl7bc.extensions.HL7Input;
import com.sun.jbi.hl7bc.extensions.HL7Output;
import com.sun.jbi.hl7bc.extensions.HL7Message;
import com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties;
import com.sun.jbi.hl7bc.extensions.HL7CommunicationControls;
import com.sun.jbi.hl7bc.util.JAXBX2JCompilerHelper;
import com.sun.jbi.hl7bc.util.JarUtil;
import com.sun.jbi.hl7bc.util.JavaCompilerHelper;
import com.sun.jbi.hl7bc.validator.EndpointValidator;
import com.sun.jbi.hl7bc.HL7Constants;
import com.sun.jbi.hl7bc.configuration.AppConfigProtocolPropertiesVisitor;
import com.sun.jbi.hl7bc.configuration.ApplicationConfigurationField;
import com.sun.jbi.hl7bc.configuration.AppConfigAddressVisitor;
import com.sun.jbi.hl7bc.configuration.Visitor;
import com.sun.jbi.hl7bc.I18n;

/**
 * WSDLConfiguration maps a list of WSDL files to the list of Endpoints based on the portmap list.
 * 
 * @author S. Nageswara Rao
 */
public class WSDLConfigurations {

	private static Logger mLogger = Logger.getLogger(WSDLConfigurations.class.getName());

	private String mRootPath;

	private List mXsds = new ArrayList();

	private Map mEncoderMap = new HashMap();

	public WSDLConfigurations(String rootPath) {
		mRootPath = rootPath;
	}

    public Collection<Endpoint> createEndpoints(
            Collection portMaps,
            Map<String, String[]> envVariableMap,
            Map<String, Collection<ApplicationConfigurationField>> appConfMap)
            throws Exception {
        
		File dir = new File(mRootPath);
        
		File catalog = new File(dir.getAbsolutePath() + File.separator
				+ "meta-inf" + File.separator + "catalog.xml");

		EntityResolver resolver = null;

		if (catalog.exists()) {
			CatalogManager catalogManager = new CatalogManager();
			catalogManager.setCatalogFiles(catalog.getAbsolutePath());
			catalogManager.setRelativeCatalogs(true);
			resolver = new CatalogResolver(catalogManager);
		}
        
		ArrayList<Endpoint> endpoints = new ArrayList<Endpoint>();
        List<File> wsdLs = listResourceFiles(dir, ".wsdl");
        mXsds = listResourceFiles(dir, ".xsd");
        for (File file : wsdLs) {
            endpoints.addAll(
                    parseWSDL(file,
                            resolver,
                            portMaps,
                            envVariableMap,
                            appConfMap));
        }
		return endpoints;
	}

    public Map<String, String[]> parseForEnvironmentVariables(List portMaps, Map<String, String[]> envVariableMap) throws Exception {
       
        Map<String, String[]> envVariables =
            new HashMap<String, String[]>(envVariableMap);
		File dir = new File(mRootPath);

		File catalog = new File(dir.getAbsolutePath() + File.separator
				+ "meta-inf" + File.separator + "catalog.xml");

		EntityResolver resolver = null;

		if (catalog.exists()) {
			CatalogManager catalogManager = new CatalogManager();
			catalogManager.setCatalogFiles(catalog.getAbsolutePath());
			catalogManager.setRelativeCatalogs(true);
			catalogManager.setUseStaticCatalog(false);
			resolver = new CatalogResolver(catalogManager);
		}
        
        for (File file : listResourceFiles(dir, ".wsdl")) {
            envVariables.putAll(
                    readWSDLForEnvVariables(file,
                            resolver,
                            envVariables));
        }

		return envVariables;
	}

	/**
     * This method parses wsdls for retrieving the acceptAckXsd and applicationAckXsd names.
     * 
     * @throws Exception
     */
	public void parseForV3ACKXsds() throws Exception {
		File dir = new File(mRootPath);

		File catalog = new File(dir.getAbsolutePath() + File.separator
				+ "meta-inf" + File.separator + "catalog.xml");

		EntityResolver resolver = null;

		if (catalog.exists()) {
			CatalogManager catalogManager = new CatalogManager();
			catalogManager.setCatalogFiles(catalog.getAbsolutePath());
			catalogManager.setRelativeCatalogs(true);
			resolver = new CatalogResolver(catalogManager);
		}
		Iterator wsdls = listResourceFiles(dir, ".wsdl").iterator();
		String str = null;
		while (wsdls.hasNext()) {
			String acceptAckXsd = readWSDLForAcceptACK((File) wsdls.next(),
					resolver);
			JAXBX2JCompilerHelper xjcHelper = new JAXBX2JCompilerHelper();
			if (acceptAckXsd != null) {
				// locate the xsd from Service Unit artifacts list
				str = locateXSDFile(dir, acceptAckXsd);
				if (str != null) {
					// generate JAXB classes - Java classes for acknowledgement
					// xsd
					try {
						str = str.replace("\\", "/");
					} catch (NullPointerException ne) {
						mLogger.log(Level.INFO, I18n.msg("I0103: Cannont the modify the string with char \\[{0}\\].",
								str));
					}
					xjcHelper.setDestDir(mRootPath);
					xjcHelper.setPackage(acceptAckXsd);
					xjcHelper.parseSchema(str);
					xjcHelper.invokeXJC();
					// compile the generated JAXB Classes
					String classesDir = mRootPath + File.separator
							+ HL7Constants.CLASSESDIR;
					File file = new File(classesDir);
					if (!file.exists())
						file.mkdir();

					List sourceFiles = listResourceFiles(new File(xjcHelper
							.getGeneratedSourceDir()), ".java");
					String[] args = constructJavaCArgs(sourceFiles);
					ByteArrayOutputStream baos = new ByteArrayOutputStream();
					boolean success = JavaCompilerHelper.compile(args, baos);
					if (!success)
						throw new Exception(I18n.msg("E0136: Compilation failed for generated JAXB source files."));
					String jarFile = mRootPath
							+ File.separator
							+ acceptAckXsd.substring(0, acceptAckXsd
									.lastIndexOf(".")) + ".jar";
					JarUtil.compress(file, new File(jarFile));
					JarUtil.setAcceptACKJarName(jarFile);
				}
			}
			String applicationAckXsd = readWSDLForApplicationACK((File) wsdls
					.next(), resolver);
			if (applicationAckXsd != null) {
				// locate the xsd from Service Unit artifacts list
				str = locateXSDFile(dir, applicationAckXsd);
				if (str != null) {
					// generate JAXB classes - Java classes for acknowledgement
					// xsd
					try {
						str = str.replace("\\", "/");
					} catch (NullPointerException ne) {
						mLogger.log(Level.INFO, I18n.msg("I0103: Cannont the modify the string with char \\[{0}\\].",
								str));
					}
					xjcHelper.setDestDir(mRootPath);
					xjcHelper.setPackage(acceptAckXsd);
					xjcHelper.parseSchema(str);
					xjcHelper.invokeXJC();
					// compile the generated JAXB Classes
					String classesDir = mRootPath + File.separator
							+ HL7Constants.CLASSESDIR;
					File file = new File(classesDir);
					if (!file.exists())
						file.mkdir();

					List sourceFiles = listResourceFiles(new File(xjcHelper
							.getGeneratedSourceDir()), ".java");
					String[] args = constructJavaCArgs(sourceFiles);
					ByteArrayOutputStream baos = new ByteArrayOutputStream();
					JavaCompilerHelper.compile(args, baos);
					String jarFile = mRootPath
							+ File.separator
							+ acceptAckXsd.substring(0, acceptAckXsd
									.lastIndexOf(".")) + ".jar";
					JarUtil.compress(file, new File(jarFile));
					JarUtil.setAppAcceptACKJarName(jarFile);
				}
			}
		}
	}

	/**
     * This method constructs the JavaC arguments as String[] and returns it
     * 
     * @param sourceFiles - List soure files
     * @return args
     */
	private String[] constructJavaCArgs(List sourceFiles) {
		String args[] = null;
		if (sourceFiles != null && sourceFiles.size() > 0) {
			args = new String[4 + sourceFiles.size()];
			args[0] = "-classpath";
			args[1] = System.getProperty("java.class.path");
			args[2] = "-d";
			args[3] = mRootPath + File.separator + HL7Constants.CLASSESDIR;
			int baseIndex = 4;
			for (int i = 0; i < sourceFiles.size(); ++i) {
				String path = (String) (((File) sourceFiles.get(i))
						.getAbsolutePath());
				args[baseIndex + i] = path;
			}
		}
		return args;

	}

      private Collection<Endpoint> parseWSDL(File wsdlFile,
                EntityResolver resolver,
                Collection portMaps,
                Map<String, String[]> envVariableMap,
                Map<String, Collection<ApplicationConfigurationField>> appConfMap)
                throws Exception {

          assert envVariableMap != null;
          assert appConfMap != null;
          assert portMaps != null;
          assert resolver != null;
          assert wsdlFile != null;  
          
		Definition def = readWSDL(wsdlFile, resolver, envVariableMap);
		ArrayList<Endpoint> endPoints = new ArrayList<Endpoint>();
         for (Object portMap : portMaps) {
			EndpointData pm = (EndpointData) portMap;

			// Check the Definition if it has an HL7Binding. If not,
			// continue
			HL7Binding binding = getHL7Binding(def, pm.getService(), pm
					.getEndpoint());
			if (binding == null) {
				continue;
			}

			// look through all the definitions to see if we can find a
			// service binding that matches this endpoint. If we find one,
			// extract file details

			// If we have an HL7Binding, we must have an HL7Address.
			HL7Address address = getHL7Address(def, pm.getService(), pm
					.getEndpoint());
			if (address == null) {
				String msg = I18n.msg("E0137: Missing hl7\\:address in wsdl for this service.")
						+ pm.getService();
				throw new Exception(msg);
			}
            
              // if an application configuration object is defined for this
            // endpoint, apply it.
            String appConfName = pm.getApplicationConfigurationObjectName();
            Collection<ApplicationConfigurationField> appConfObj = appConfMap.get(appConfName);
            if (appConfObj != null) {
                applyApplicationConfiguration(address, appConfObj);
            }

			HL7ProtocolProperties protocolproperties = getHL7ProtocolProperties(
					def, pm.getService(), pm.getEndpoint());
			if (protocolproperties == null) {
				String msg = I18n.msg("E0138: Missing hl7\\:protocolproperties in wsdl for this service.")
						+ pm.getService();
				throw new Exception(msg);
			}
			
            if (appConfObj != null) {
                applyApplicationConfiguration(protocolproperties, appConfObj);
            }

			// see whether any communicationcontrols are configured
            HL7CommunicationControls communicationControls = getHL7CommunicationControls(
                    def, pm.getService(), pm.getEndpoint()); 

			// If we have an HL7Binding, we must have operations
			Map hl7Operations = getHL7Operations(def, pm.getService(), pm
					.getEndpoint());

			if (hl7Operations == null || hl7Operations.size() == 0) {
				String msg = I18n.msg("E0139: Missing hl7\\:operation definition(s) in wsdl for this service.")
						+ pm.getService();
				throw new Exception(msg);
			}

			Map opMeps = getHL7OperationMepType(def, pm.getService(), pm
					.getEndpoint(), pm.getDirection());

			// Create an Endpoint for each Port. The endpoint should have the
			// correct HL7Binding, the associated Operations, and the
			// associated OperationInput and OperationOutput
			Endpoint endpoint = new EndpointImpl();

			// Store the endpoint name
			endpoint.setEndpointName(pm.getEndpoint());

			// Store the Service name
			endpoint.setServiceName(QName.valueOf(pm.getService()));

			// Store the Definition
			endpoint.setDefinition(def);

			// Store the type of Endpoint
			endpoint.setEndpointType(pm.getDirection());

			// Set the description
			DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory
					.newInstance();
			DocumentBuilder documentBuilder = docBuilderFactory
					.newDocumentBuilder();
			Document result = documentBuilder.parse(wsdlFile);
			endpoint.setServiceDescription(result);
			endpoint.setServiceUnitPath(mRootPath);

			// Store our extensibility elements
			endpoint.setHL7Address(address);
			endpoint.setHL7ProtocolProperties(protocolproperties);
			// for recourse actions
            if(communicationControls != null) {
                endpoint.setHL7CommunicationControls(communicationControls);
            }
			endpoint.setHL7Binding(binding);
			endpoint.setHL7Operations(hl7Operations);

			endpoint.setOperationMsgExchangePattern(opMeps);
			setInputsOutputs(def, pm.getService(), pm.getEndpoint(),
					hl7Operations.values(), endpoint);
			// This list gets utilized while generating NAK/ACK from HL7 BC
			endpoint.setXsdsList(mXsds);

			EndpointValidator.validateEndpointForUniqueness(endPoints,
					endpoint, false);
			endPoints.add(endpoint);
		}

		return endPoints;
	}
      
      /**
         * Applies the configuration values represented by an application configuration object, to
         * an HL7Address object.
         * 
         * @param address The HL7Address to which to apply the configuration
         * @param appConfObj The application configuration object to apply
         */
      private void applyApplicationConfiguration(HL7Address address,
                                                 Collection<ApplicationConfigurationField> appConfObj) {
          assert address != null;
          assert appConfObj != null;
          if (!appConfObj.isEmpty()) {
              Visitor addressVisitor = new AppConfigAddressVisitor(address);
              for (ApplicationConfigurationField field: appConfObj) {
                  field.accept(addressVisitor);
              }
          }
      }
      
      /**
       * Applies the configuration values represented by an application configuration object, to
       * an HL7ProtocolProperties object.
       * 
       * @param address The HL7ProtocolProperties to which to apply the configuration
       * @param appConfObj The application configuration object to apply
       */
    private void applyApplicationConfiguration(HL7ProtocolProperties protocolproperties,
                                               Collection<ApplicationConfigurationField> appConfObj) {
        assert protocolproperties != null;
        assert appConfObj != null;
        if (!appConfObj.isEmpty()) {
            Visitor protocolPropertiesVisitor = new AppConfigProtocolPropertiesVisitor(protocolproperties);
            for (ApplicationConfigurationField field: appConfObj) {
                field.accept(protocolPropertiesVisitor);
            }
        }
    }      
      
      /**
         * List all wsdl files in the currentDir and below
         */
      private List<File> listResourceFiles(final File currentDir, final String extension) {
          final List<File> cumulativeResults = new ArrayList<File>();
          final File[] filesInCurrentDir = currentDir.listFiles();
          for (final File element : filesInCurrentDir) {
              
              if (element.isFile()) {
                  if (element.getName().toLowerCase().endsWith(extension)) {
                      cumulativeResults.add(element);
                  }
              } else if (element.isDirectory()) {
                  final List<File> filesInSubdirectories =
                          listResourceFiles(element, extension);
                  cumulativeResults.addAll(filesInSubdirectories);
              }
          }
          return cumulativeResults;
      }


	

	private String locateXSDFile(File currentDir, String xsdFileName) {
		String xsdFilePath = null;
		File[] filesInCurrentDir = currentDir.listFiles();
		for (int fileCount = 0; fileCount < filesInCurrentDir.length; fileCount++) {

			if (filesInCurrentDir[fileCount].isFile()) {
				if (filesInCurrentDir[fileCount].getName().equals(xsdFileName)) {
					xsdFilePath = filesInCurrentDir[fileCount]
							.getAbsolutePath();
					return xsdFilePath;
				}
			} else if (filesInCurrentDir[fileCount].isDirectory()) {
				// String path = locateXSDFile(filesInCurrentDir[fileCount],
				// xsdFileName);
				// xsdFilePath = path;
				String path = locateXSDFile(filesInCurrentDir[fileCount],
						xsdFileName);

				if (path != null && path.endsWith(xsdFileName))
					xsdFilePath = path;

			}
		}
		return xsdFilePath;
	}

	private Definition readWSDL(File f, EntityResolver resolver,
                                Map<String, String[]> envVariableMap) throws WSDLException {
		WSDLFactory wsdlFactory = WSDLFactory.newInstance();
		WSDLReader reader = resolver == null ? ((WSDLFactoryImpl) wsdlFactory)
				.newWSDLReader() : ((WSDLFactoryImpl) wsdlFactory)
				.newWSDLReader(resolver);
		reader.setExtensionRegistry(new HL7ExtensionRegistry(envVariableMap));
		Definition def = reader.readWSDL(f.getAbsolutePath());

		return def;
	}

	private Map readWSDLForEnvVariables(File f, EntityResolver resolver,
                                        Map<String, String[]> envVariableMap) throws WSDLException {
		WSDLFactory wsdlFactory = WSDLFactory.newInstance();
		WSDLReader reader = resolver == null ? ((WSDLFactoryImpl) wsdlFactory)
				.newWSDLReader() : ((WSDLFactoryImpl) wsdlFactory)
				.newWSDLReader(resolver);

		HL7ExtPreprocessDeserializer preProcessDeserializer = new HL7ExtPreprocessDeserializer(
				envVariableMap);
		reader.setExtensionRegistry(new HL7ExtensionRegistry(
				preProcessDeserializer));
		reader.readWSDL(f.getAbsolutePath());
		return preProcessDeserializer.getEnvVariableMap();
	}

	private String readWSDLForAcceptACK(File f, EntityResolver resolver)
			throws WSDLException {
		WSDLFactory wsdlFactory = WSDLFactory.newInstance();
		WSDLReader reader = resolver == null ? ((WSDLFactoryImpl) wsdlFactory)
				.newWSDLReader() : ((WSDLFactoryImpl) wsdlFactory)
				.newWSDLReader(resolver);
		HL7ExtPreprocessDeserializer preProcessDeserializer = new HL7ExtPreprocessDeserializer();
		reader.setExtensionRegistry(new HL7ExtensionRegistry(
				preProcessDeserializer));
		reader.readWSDL(f.getAbsolutePath());
		return preProcessDeserializer.getAcceptAckXsdName();
	}

	private String readWSDLForApplicationACK(File f, EntityResolver resolver)
			throws WSDLException {
		WSDLFactory wsdlFactory = WSDLFactory.newInstance();
		WSDLReader reader = resolver == null ? ((WSDLFactoryImpl) wsdlFactory)
				.newWSDLReader() : ((WSDLFactoryImpl) wsdlFactory)
				.newWSDLReader(resolver);
		HL7ExtPreprocessDeserializer preProcessDeserializer = new HL7ExtPreprocessDeserializer();
		reader.setExtensionRegistry(new HL7ExtensionRegistry(
				preProcessDeserializer));
		reader.readWSDL(f.getAbsolutePath());
		return preProcessDeserializer.getApplicationAckXsdName();
	}

	protected Binding getBinding(Definition def, String serviceName,
			String endpointName) {

		// It checks all imported WSDLs.
		Map services = def.getServices();
		Service svc = (Service) services.get(QName.valueOf(serviceName));
		if (svc == null) {
			return null;
		}
		Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
		if (port == null) {
			return null;
		} else {
			return port.getBinding();
		}
	}

	protected HL7Address getHL7Address(Definition def, String serviceName,
			String endpointName) {
		HL7Address address = null;
		Service svc = def.getService(QName.valueOf(serviceName));
		if (svc == null) {
			return null;
		}
		Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
		if (port != null) {
			List extElems = port.getExtensibilityElements();

			// Look for hl7:address

			Iterator extIter = extElems == null ? null : extElems.iterator();
			while (extIter != null && extIter.hasNext() && address == null) {
				ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
				if (HL7Address.class.isInstance(ee)) {
					address = (HL7Address) ee;
				}
			}
		}
		return address;
	}

	protected HL7ProtocolProperties getHL7ProtocolProperties(Definition def,
			String serviceName, String endpointName) {
		HL7ProtocolProperties protocolProperties = null;
		Service svc = def.getService(QName.valueOf(serviceName));
		if (svc == null) {
			return null;
		}
		Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
		if (port != null) {
			List extElems = port.getExtensibilityElements();

			// Look for hl7:protocolproperties

			Iterator extIter = extElems == null ? null : extElems.iterator();
			while (extIter != null && extIter.hasNext()
					&& protocolProperties == null) {
				ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
				if (HL7ProtocolProperties.class.isInstance(ee)) {
					protocolProperties = (HL7ProtocolProperties) ee;
				}
			}
		}
		return protocolProperties;

	}

	protected HL7CommunicationControls getHL7CommunicationControls(
			Definition def, String serviceName, String endpointName) {
		HL7CommunicationControls communicationControls = null;
		Service svc = def.getService(QName.valueOf(serviceName));
		if (svc == null) {
			return null;
		}
		Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
		if (port != null) {
			List extElems = port.getExtensibilityElements();

			// Look for hl7:communicationcontrols

			Iterator extIter = extElems == null ? null : extElems.iterator();
			while (extIter != null && extIter.hasNext()
					&& communicationControls == null) {
				ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
				if (HL7CommunicationControls.class.isInstance(ee)) {
					communicationControls = (HL7CommunicationControls) ee;
				}
			}
		}
		return communicationControls;

	}

	protected HL7Binding getHL7Binding(Definition def, String serviceName,
			String endpointName) {
		HL7Binding hl7Binding = null;
		Binding binding = getBinding(def, serviceName, endpointName);
		if (binding != null) {
			List extElems = binding.getExtensibilityElements();

			// Look for hl7:binding

			Iterator extIter = extElems == null ? null : extElems.iterator();
			while (extIter != null && extIter.hasNext() && hl7Binding == null) {
				ExtensibilityElement ee = (ExtensibilityElement) extIter.next();
				if (HL7Binding.class.isInstance(ee)) {
					hl7Binding = (HL7Binding) ee;
				}
			}
		}
		return hl7Binding;
	}

	protected Map getHL7Operations(Definition def, String serviceName,
			String endpointName) {
		Map hl7Operations = new HashMap();
		Binding binding = getBinding(def, serviceName, endpointName);
		if (binding != null) {
			final PortType portType = binding.getPortType();
			List bindingOperations = binding.getBindingOperations();
			Iterator operIter = bindingOperations == null ? null
					: bindingOperations.iterator();
			while (operIter != null && operIter.hasNext()) {
				BindingOperation oper = (BindingOperation) operIter.next();
				List extElems = oper.getExtensibilityElements();
				// Look for hl7:operation entries

				Iterator extIter = extElems == null ? null : extElems
						.iterator();
				while (extIter != null && extIter.hasNext()) {
					ExtensibilityElement ee = (ExtensibilityElement) extIter
							.next();
					if (HL7Operation.class.isInstance(ee)) {
						HL7Operation hl7Operation = (HL7Operation) ee;
						BindingInput bindingInput = oper.getBindingInput();
						if (bindingInput != null) {
							Iterator inputIter = bindingInput
									.getExtensibilityElements().iterator();
							while (inputIter.hasNext()) {
								ExtensibilityElement inputExt = (ExtensibilityElement) inputIter
										.next();
								HL7Input hl7Input = null;
								if (inputExt instanceof HL7Message) {
									hl7Input = new HL7Input();
									HL7Message hl7Message = (HL7Message) inputExt;
									hl7Input.setHL7Message(hl7Message);
								}
								if (hl7Input != null) {
									hl7Operation.setHL7OperationInput(hl7Input);
								}
							}
						}

						BindingOutput bindingOutput = oper.getBindingOutput();
						if (bindingOutput != null) {
							Iterator outputIter = bindingOutput
									.getExtensibilityElements().iterator();
							while (outputIter.hasNext()) {
								ExtensibilityElement outputExt = (ExtensibilityElement) outputIter
										.next();
								HL7Output hl7Output = null;
								if (outputExt instanceof HL7Message) {
									hl7Output = new HL7Output();
									HL7Message hl7Message = (HL7Message) outputExt;
									hl7Output.setHL7Message(hl7Message);
								}
								if (hl7Output != null) {
									hl7Operation
											.setHL7OperationOutput(hl7Output);
								}
							}
						}
						// hl7Operations.put(QName.valueOf(oper.getName()),
						// hl7Operation);
						hl7Operations.put(new QName(portType.getQName()
								.getNamespaceURI(), oper.getName()),
								hl7Operation);
					}
				}
			}
		}
		return hl7Operations;
	}

	protected void setInputsOutputs(Definition def, String serviceName,
			String endpointName, Collection operations, Endpoint endpoint) {

		Binding binding = getBinding(def, serviceName, endpointName);
		if (binding != null) {
			List bindingOperations = binding.getBindingOperations();
			Iterator operIter = bindingOperations == null ? null
					: bindingOperations.iterator();
			while (operIter != null && operIter.hasNext()) {
				BindingOperation oper = (BindingOperation) operIter.next();
				List extElems = oper.getExtensibilityElements();
				// Look for hl7:operation entries

				HL7Operation hl7Operation = null;
				Iterator extIter = extElems == null ? null : extElems
						.iterator();
				while (extIter != null && extIter.hasNext()) {
					ExtensibilityElement ee = (ExtensibilityElement) extIter
							.next();
					if (operations.contains(ee)) {
						hl7Operation = (HL7Operation) ee;
					}
				}

				if (hl7Operation != null) {
					HL7Input hl7Input = hl7Operation.getHL7OperationInput();
					if (hl7Input != null) {
						endpoint.setHL7OperationInput(hl7Operation, hl7Input);
					}
					HL7Output hl7Output = hl7Operation.getHL7OperationOutput();
					if (hl7Output != null) {
						endpoint.setHL7OperationOutput(hl7Operation, hl7Output);
					}

				}
			}
		}
	}

	protected Map getHL7OperationMepType(Definition def, String serviceName,
			String endpointName, int direction) {
		Map mepTypes = new HashMap();
		Binding binding = getBinding(def, serviceName, endpointName);
		if (binding != null) {
			final PortType portType = binding.getPortType();
			List bindingOperations = binding.getBindingOperations();
			Iterator operIter = bindingOperations == null ? null
					: bindingOperations.iterator();
			while (operIter != null && operIter.hasNext()) {
				BindingOperation oper = (BindingOperation) operIter.next();
				List extElems = oper.getExtensibilityElements();

				Iterator extIter = extElems == null ? null : extElems
						.iterator();
				while (extIter != null && extIter.hasNext()) {
					ExtensibilityElement ee = (ExtensibilityElement) extIter
							.next();
					if (HL7Operation.class.isInstance(ee)) {
						String mep = Endpoint.EndpointMessageType.UNSUPPORTED;
						if (oper.getOperation().getStyle() != null) {
							mep = determineMEP(direction, oper);
						} else {
							// for some reason, sometimes the operation type is
							// not set
							// so the BC will populate it based on the WSDL
							// definition
							// for now, we only handle Request-response and
							// one-way.
							// anything else is not supported and should be
							// caught in WSDL
							// validation
							Operation operation = oper.getOperation();
							OperationType type = null;
							if (operation.getInput() != null
									&& operation.getOutput() != null) {
								type = OperationType.REQUEST_RESPONSE;
							} else if (operation.getInput() != null) {
								type = OperationType.ONE_WAY;
							}
							if (type != null) {
								mep = determineMEP(direction, type);
							}
						}
						// mepTypes.put(QName.valueOf(oper.getName()), mep);
						mepTypes.put(new QName(portType.getQName()
								.getNamespaceURI(), oper.getName()), mep);
					}
				}
			}
		}
		return mepTypes;
	}

	public Map getPartEncoderMapping(Definition def, String serviceName,
			String endpointName, int direction, Map hl7Operations, Map mepTypes)
			throws Exception {
		Map partMapping = new HashMap();

		// Don't support inline encoder schemas
		if (mXsds.size() <= 0) {
			return partMapping;
		}

		Service service = def.getService(QName.valueOf(serviceName));
		Port port = service.getPort(endpointName);
		PortType portType = port.getBinding().getPortType();

		/**
         * Locate the operation we are interested in. There may be multiple operations by the same
         * name (operation overloading) and the WSDL spec does not allow it. The uniqueness should
         * be guaranteed by the examination of input and/or output names. The WSDL validation
         * should've been enforced the uniqueness at design time. For the time being, we will assume
         * that we don't have operation overloading.
         */
		javax.wsdl.Message wsdlMessage = null;
		Map parts = new HashMap();
		for (Iterator opnameIter = hl7Operations.keySet().iterator(); opnameIter
				.hasNext();) {
			String mep = null;
			String encodingStyle = null;

			QName operationName = (QName) opnameIter.next();
			HL7Operation hl7Operation = (HL7Operation) hl7Operations
					.get(operationName);
			mep = (String) mepTypes.get(operationName);

			for (Iterator operIter = portType.getOperations().iterator(); operIter
					.hasNext();) {
				Operation op = (Operation) operIter.next();
				if (op.getName().equals(operationName.toString())
						|| op.getName().equals(operationName.getLocalPart())) {
					/**
                     * There is nothing in the WSDL spec that says that the part name has to be
                     * unique within the WSDL document, so we need to prefix the part name with the
                     * message name.
                     */

					Input input = op.getInput();
					if (input != null) {
						HL7Input hl7Input = hl7Operation.getHL7OperationInput();
						if (hl7Input == null) {
							throw new Exception(I18n.msg("E0140: Missing required hl7:Input for hl7 binding operation \\[{0}\\].",
								operationName.toString()));
						}
						wsdlMessage = input.getMessage();
						parts = wsdlMessage.getParts();
						for (Iterator partIter = parts.values().iterator(); partIter
								.hasNext();) {
							Part aPart = (Part) partIter.next();
							QName type = (aPart.getElementName() != null) ? aPart
									.getElementName()
									: aPart.getTypeName();
							String partName = aPart.getName();

							// locate the XSD file based on the part type
							// namespace
							String xsdFileLoc = getXsdFileLocation(type);
							if (xsdFileLoc != null) {
								// Determine on which extensibility element the
								// encoding style is
								// defined
								if (direction == Endpoint.EndpointType.INBOUND) { // Consumer
									HL7Message hL7Message = hl7Input
											.getHL7Message();
									if (hL7Message == null) {
										throw new Exception(I18n.msg("E0141: Missing required hl7:message for hl7 binding operation \\[{0}\\].",
														operationName.toString()));
									}
									if (hL7Message.getUseType().equals(
											HL7Message.ATTR_USE_TYPE_ENCODED)) {
										encodingStyle = hL7Message
												.getEncodingStyle();
									} else {
										continue;
									}
									if (encodingStyle == null
											|| encodingStyle.equals("")) {
										throw new Exception(
												I18n.msg("E0142: Missing required hl7:message 'encodingStyle' attribute(value) for hl7 binding operation \\[{0}\\] while 'use' is 'encoded'.",
														operationName.toString()));
									}
								} else { // Provider
									HL7Message hL7Message = hl7Input
											.getHL7Message();
									if (hL7Message == null) {
										throw new Exception(I18n.msg("E0141: Missing required hl7:message for hl7 binding operation \\[{0}\\].",
														operationName.toString()));
									}
									if (hL7Message.getUseType().equals(
											HL7Message.ATTR_USE_TYPE_ENCODED)) {
										encodingStyle = hL7Message
												.getEncodingStyle();
									} else {
										continue;
									}

									if (encodingStyle == null
											|| encodingStyle.equals("")) {
										throw new Exception(I18n.msg("E0142: Missing required hl7:message 'encodingStyle' attribute(value) for hl7 binding operation \\[{0}\\] while 'use' is 'encoded'.",
																operationName.toString()));
									}
								}

								Encoder encoder = null;
								MetaRef metaRef = new MyMetaRef(xsdFileLoc,
										type);
								if (mEncoderMap.get(metaRef) != null) {
									encoder = (Encoder) mEncoderMap
											.get(metaRef);
								} else {
									EncoderFactory encoderFactory = EncoderFactory
											.newInstance();
									encoder = encoderFactory.newEncoder(
											encoderFactory
													.makeType(encodingStyle),
											metaRef);
									mEncoderMap.put(metaRef, encoder);
								}
								partMapping.put(wsdlMessage.getQName()
										+ partName, encoder);
							}
						}
					}

					Output output = op.getOutput();
					if (output != null) {
						HL7Output hl7Output = hl7Operation
								.getHL7OperationOutput();
						if (hl7Output == null) {
							throw new Exception(I18n.msg(
									"E0143: Missing required Output properties for hl7 binding operation \\[{0}\\].", operationName
											.toString()));
						}

						wsdlMessage = output.getMessage();
						parts = wsdlMessage.getParts();
						for (Iterator partIter = parts.values().iterator(); partIter
								.hasNext();) {
							Part aPart = (Part) partIter.next();
							String partName = aPart.getName();
							QName type = (aPart.getElementName() != null) ? aPart
									.getElementName()
									: aPart.getTypeName();

							// locate the XSD file based on the part type
							// namespace
							String xsdFileLoc = getXsdFileLocation(type);
							if (xsdFileLoc != null) {
								if (direction == Endpoint.EndpointType.INBOUND
										&& mep
												.equals(Endpoint.EndpointMessageType.IN_OUT)) {
									// Consumer request-response operations
									HL7Message hl7Message = hl7Output
											.getHL7Message();
									if (hl7Message == null) {
										throw new Exception(I18n.msg("E0141: Missing required hl7:message for hl7 binding operation \\[{0}\\].",
														operationName
																.toString()));
									}

									if (hl7Message.getUseType().equals(
											HL7Message.ATTR_USE_TYPE_ENCODED)) {
										encodingStyle = hl7Message
												.getEncodingStyle();
									} else {
										continue;
									}
									if (encodingStyle == null
											|| encodingStyle.equals("")) {
										throw new Exception(I18n.msg("E0142: Missing required hl7:message 'encodingStyle' attribute(value) for hl7 binding operation \\[{0}\\] while 'use' is 'encoded'.",
																operationName.toString()));
									}
								} else { // Provider
									HL7Message hl7Message = hl7Output
											.getHL7Message();
									if (hl7Message == null) {
										throw new Exception(I18n.msg("E0141: Missing required hl7:message for hl7 binding operation \\[{0}\\].",
														operationName.toString()));
									}

									if (hl7Message.getUseType().equals(
											HL7Message.ATTR_USE_TYPE_ENCODED)) {
										encodingStyle = hl7Message
												.getEncodingStyle();
									} else {
										continue;
									}
									if (encodingStyle == null
											|| encodingStyle.equals("")) {
										throw new Exception(I18n.msg("E0142: Missing required hl7:message 'encodingStyle' attribute(value) for hl7 binding operation \\[{0}\\] while 'use' is 'encoded'.",
																operationName.toString()));
									}
								}

								Encoder encoder = null;
								MetaRef metaRef = new MyMetaRef(xsdFileLoc,
										type);
								if (mEncoderMap.get(metaRef) != null) {
									encoder = (Encoder) mEncoderMap
											.get(metaRef);
								} else {
									EncoderFactory encoderFactory = EncoderFactory
											.newInstance();
									encoder = encoderFactory.newEncoder(
											encoderFactory
													.makeType(encodingStyle),
											metaRef);
									mEncoderMap.put(metaRef, encoder);
								}
								partMapping.put(wsdlMessage.getQName()
										+ partName, encoder);
							}
						}

					}
					break;
				}
			}
		}

		return partMapping;
	}

	public void clearEncoderCache() {
		mEncoderMap.clear();
	}

	protected String getXsdFileLocation(QName elemName) {
		if (elemName == null) {
			return null;
		}

		File aXsdFile = null;
		try {
			aXsdFile = XsdLocator.findXsdByElement(mXsds, elemName);
		} catch (Exception e) {
			mLogger.log(Level.SEVERE, I18n.msg(
					"E0144: Parsing failed for {0}. {1}", aXsdFile.getName(),
							e.getLocalizedMessage()), e);
		}
		return aXsdFile != null ? aXsdFile.getAbsolutePath() : null;
	}

	/**
     * Given the list of xsds, return exact xsd file where inputted element is present
     * 
     * @param elemName The element for which the schemas are scanned for existence
     * @param xsds List of xsds
     * @return String the schema location
     */
	public static String getXsdFileLocation(QName elemName, List xsds) {
		if (elemName == null) {
			return null;
		}

		File aXsdFile = null;
		try {
			aXsdFile = XsdLocator.findXsdByElement(xsds, elemName);
		} catch (Exception e) {
			mLogger.log(Level.SEVERE, I18n.msg(
					"E0144: Parsing failed for {0}. {1}", aXsdFile.getName(),
							e.getLocalizedMessage() ), e);
		}
		return aXsdFile != null ? aXsdFile.getAbsolutePath() : null;
	}

	/**
     * Determine the message exchange pattern. For handling 1.1 wsdls, map transmission primitives
     * to the closest message exchange pattern, taking into account the endpoint direction direction
     * inbound: request-response and solicit-response -> in-out one-way and notification -> in-only
     * direction outbound: request-response and solicit-response -> out-in one-way and notification ->
     * out-only
     * 
     * @param pm the endpoint configuration from the portmap
     * @param po the binding operation definition from the wsdl
     * @return the message exchange pattern, null if no mapping could be determined.
     */
	protected String determineMEP(int direction, BindingOperation bo) {
		String mep = null;
		OperationType type = bo.getOperation().getStyle();

		if (direction == Endpoint.EndpointType.INBOUND) {
            if (type.equals(OperationType.REQUEST_RESPONSE)) {
                mep = Endpoint.EndpointMessageType.IN_OUT;
            } else if (type.equals(OperationType.ONE_WAY)) {
				mep = Endpoint.EndpointMessageType.IN_ONLY;
			} else {
				mep = Endpoint.EndpointMessageType.UNSUPPORTED;
			}
		} else {
			if (type == null) {
				// Not sure why type is not always populated for outbound
				// binding operations.
				// Returning UNSUPPORTED
				return Endpoint.EndpointMessageType.UNSUPPORTED;
			}
			if (type.equals(OperationType.REQUEST_RESPONSE)) {
				mep = Endpoint.EndpointMessageType.OUT_IN;
			} else if (type.equals(OperationType.ONE_WAY)) {
				mep = Endpoint.EndpointMessageType.OUT_ONLY;
			} else {
				mep = Endpoint.EndpointMessageType.UNSUPPORTED;
			}
		}
		return mep;
	}

	protected String determineMEP(int direction, OperationType type) {
		String mep = null;

		if (direction == Endpoint.EndpointType.INBOUND) {
            if (type.equals(OperationType.REQUEST_RESPONSE)) {
                mep = Endpoint.EndpointMessageType.IN_OUT;
            } else if (type.equals(OperationType.ONE_WAY)) {
				mep = Endpoint.EndpointMessageType.IN_ONLY;
			} else {
				mep = Endpoint.EndpointMessageType.UNSUPPORTED;
			}
		} else {
			if (type.equals(OperationType.REQUEST_RESPONSE)) {
				mep = Endpoint.EndpointMessageType.OUT_IN;
			} else if (type.equals(OperationType.ONE_WAY)) {
				mep = Endpoint.EndpointMessageType.OUT_ONLY;
			} else {
				mep = Endpoint.EndpointMessageType.UNSUPPORTED;
			}
		}
		return mep;
	}

	/**
     * An implementation of the MetaRef interface
     */
	private class MyMetaRef implements MetaRef {
		private final String mXsdPath;

		private final QName mRootElemName;

		private final String mToString;

		/**
         * Constructor
         */
		protected MyMetaRef(String xsdLoc) {
			this(xsdLoc, null);
		}

		/**
         * Alternative constructor that constructs a MetaRef object with the file path location of
         * the main XSD and qualified name of the root element
         */
		protected MyMetaRef(String xsdLoc, QName rootElemName) {
			mXsdPath = xsdLoc;
			mRootElemName = rootElemName;
			mToString = toString();
		}

		/**
         * Return the file path location of the main XSD
         * 
         * @return the path of the main meta file
         */
		public String getPath() {
			return mXsdPath;
		}

        /**
         * Gets the URL of the main metadata file. This URL should point to an XSD file somewhere.
         * If this method returns a value other than <code>null</code>, the return value of
         * <code>getPath()</code> will be ignored. To load encoder metadata from a jar file, a URL
         * in form "jar:&lt;url&gt;!/{entry}" can be used.
         * 
         * @return the URL of the main meta file
         */

		public URL getURL() {
			return null;
		}

		/**
         * Return the QName of the root element.
         * 
         * @return the QName of the root element
         */
		public QName getRootElemName() {
			return mRootElemName;
		}

		public String toString() {
			return mXsdPath + mRootElemName.toString();
		}

		public boolean equals(Object obj) {
			if (!(obj instanceof MyMetaRef)) {
				return false;
			}
			return mToString.equals(((MyMetaRef) obj).mToString);
		}

		public int hashCode() {
			return mToString.hashCode();
		}
	}

}
