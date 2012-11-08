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
 * @(#)AspectActionProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.aspects.web.ajax.servlet;

import com.sun.jbi.cam.plugins.aspects.support.model.policygroup.xml.AspectPolicyGroupReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Serializable;
import java.io.SyncFailedException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;

import org.apache.xmlbeans.XmlException;
import org.xml.sax.SAXException;

import com.sun.jbi.cam.common.FileUtilities;
import com.sun.jbi.cam.common.JarFileUtility;
import com.sun.jbi.cam.common.ReadWriteTextFile;
import com.sun.jbi.cam.common.StringHelper;
import com.sun.jbi.cam.plugins.aspects.common.XmlConstants;
import com.sun.jbi.cam.plugins.aspects.support.model.Advice;
import com.sun.jbi.cam.plugins.aspects.support.model.Aspect;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectAdvice;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectMap;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectMapCreator;
import com.sun.jbi.cam.plugins.aspects.support.model.AspectOutput;
import com.sun.jbi.cam.plugins.aspects.support.model.FacadeConfiguration;
import com.sun.jbi.cam.plugins.aspects.support.model.FacadeServicesInformation;
import com.sun.jbi.cam.plugins.aspects.support.model.PartnerConfiguration;
import com.sun.jbi.cam.plugins.aspects.support.model.ProviderConfiguration;
import com.sun.jbi.cam.plugins.aspects.support.model.ProviderServicesInformation;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.Catalog;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.WSDLService;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.xml.CatalogReader;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.xml.CatalogWriter;
import com.sun.jbi.cam.plugins.aspects.support.model.policygroup.PolicyGroup;
import com.sun.jbi.cam.plugins.aspects.support.model.policygroup.PolicyGroupCollection;
import com.sun.jbi.cam.plugins.aspects.support.model.policygroup.xml.AspectPolicyGroupWriter;
import com.sun.jbi.cam.plugins.aspects.support.model.xml.AdviceReader;
import com.sun.jbi.cam.plugins.aspects.support.model.xml.AspectMapReader;
import com.sun.jbi.cam.plugins.aspects.support.model.xml.AspectMapWriter;
import com.sun.jbi.cam.plugins.aspects.support.model.xml.BindingServiceUnitDescriptorGenerator;
import com.sun.jbi.cam.plugins.aspects.support.model.xml.EngineServiceUnitDescriptorGenerator;
import com.sun.jbi.cam.plugins.aspects.support.model.xml.FacadeInformationReader;
import com.sun.jbi.cam.plugins.aspects.support.model.xml.FacadeWSDLCreator;
import com.sun.jbi.cam.plugins.aspects.support.model.xml.ServiceAssemblyDescriptorGenerator;
import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.WSDLModel;
import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.WSDLModelHelper;
import com.sun.jbi.ui.client.JBIAdminCommandsClientFactory;
import com.sun.jbi.ui.common.JBIAdminCommands;
import com.sun.jbi.ui.common.JBIRemoteException;

/**
 * 
 * @author graj
 * @version
 */
public class AspectActionProcessor extends HttpServlet implements Serializable {
	private static final long serialVersionUID = 1L;

	/**
	 * Processes requests for both HTTP <code>GET</code> and <code>POST</code>
	 * methods.
	 * 
	 * @param request
	 *            servlet request
	 * @param response
	 *            servlet response
	 */
	protected void processRequest(HttpServletRequest request,
			HttpServletResponse response) throws ServletException, IOException {
		final String POLICYGROUPS_URL = "/faces/aspects/policygroups.jsp";
		final String XMLEXTENSION_KEY = ".xml";

		final String WORKSPACE_FOLDERNAME_KEY = "workspace";
		final String CATALOG_FOLDERNAME_KEY = "catalog";
		final String ASPECTPOLICYGROUPS_FOLDERNAME_KEY = "aspectpolicygroups";
		final String SRC_FOLDERNAME_KEY = "src";
		final String BUILD_FOLDERNAME_KEY = "build";
		final String DIST_FOLDERNAME_KEY = "dist";
		final String META_INF_FOLDERNAME_KEY = "META-INF";
		final String ASPECTSERVICEUNITNAME_KEY = "AspectPolicyUnit";
		final String HTTPSERVICEUNITNAME_KEY = "sun-http-binding";
                PolicyGroupCollection policyGroupCollection = new PolicyGroupCollection();
		PolicyGroup policyGroup = new PolicyGroup();

		HttpSession session = request.getSession(false);
		if (null == session) {
			System.out.println("Invalid Session...");
			System.out.println("Redirecting to " + POLICYGROUPS_URL);
			response.sendRedirect(POLICYGROUPS_URL);
		}
		System.out.println("Start of processing action");
		ServletContext context = getServletContext();
		String rootPath = context.getRealPath("/");

		String key = null;
		String value = null;
		String policyGroupName = null;

		AspectMapReader aspectMapReader = null;
		ProviderServicesInformation providerServicesInformation = new ProviderServicesInformation();
		ProviderConfiguration providerConfiguration = null;

		FacadeInformationReader facadeInformationReader = null;
		FacadeServicesInformation facadeServicesInformation = new FacadeServicesInformation();
		FacadeConfiguration facadeConfiguration = null;

		List<AspectAdvice> adviceList = new ArrayList<AspectAdvice>();

		Map<String /* fileName */, String /* configString */> nameToConfigMap = new HashMap<String, String>();

		// ///////////////////////////////////////////////////////
		// 0. Read user input
		// ///////////////////////////////////////////////////////
		Map<String /* key */, String[] /* values */> parameterMap = null;
		parameterMap = request.getParameterMap();
		for (Entry<String /* key */, String[] /* values */> entry : parameterMap
				.entrySet()) {
			key = entry.getKey();
			if (key.startsWith(XmlConstants.ASPECTPOLICYGROUP_POLICYGROUPNAME_STARTSWITH_KEY)) {
				// group name
				// policyGroupName
				policyGroupName = entry.getValue()[0];
				System.out.println("***** In policyGroupName block - "
						+ "Key is: " + key + " value is: " + policyGroupName);
				policyGroup.setPolicyGroupName(policyGroupName);
			} else if (key.startsWith(XmlConstants.ASPECTPOLICYGROUP_EXPORT_OPERATION_STARTSWITH_KEY)) {
				// aspect map
				// export_operation
				String rawXMLData = entry.getValue()[0];
				System.out.println("***** In operation block - " + "Key is: "
						+ key + " value is: " + rawXMLData);

				try {
					aspectMapReader = AspectMapReader
							.parseFromXMLData(rawXMLData);
					AspectMap aspectMap = aspectMapReader.getAspectMap();
					List<Aspect> aspectList = aspectMap.getAspectList();
					for (Aspect aspect : aspectList) {
						List<AspectOutput> outputList = aspect.getOutputList();
						for (AspectOutput output : outputList) {
							System.out.println(output.getServiceQName());
							System.out.println(output.getPortName());
							System.out.println(output.getPortTypeQName());
							System.out.println(output.getOperationName());
							System.out.println(output.getMessageTypeQName());
							System.out.println(output.getInputTransformation());
							System.out.println(output.getId());
							providerConfiguration = new ProviderConfiguration(
									output.getServiceQName(), output
											.getPortName(), output
											.getPortTypeQName());
							providerServicesInformation
									.addProviderConfiguration(providerConfiguration);
						}
						Map<Integer, AspectAdvice> orderToAdviceMap = aspect
								.getOrderToAdviceMap();
						for (Entry<Integer, AspectAdvice> entryElement : orderToAdviceMap
								.entrySet()) {
							if (entryElement != null) {
								Integer order = entryElement.getKey();
								AspectAdvice advice = entryElement.getValue();
								if (advice != null) {
									adviceList.add(advice);
								}
							}
						}
					}
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
				}
			} else if (key
					.indexOf(XmlConstants.ASPECTPOLICYGROUP_FACADE_INDEXOF_KEY) > 0) {
				// facade adviceConfiguration
				// export_asp8_facade_1
				String rawXMLData = entry.getValue()[0];
				System.out.println("***** In facade block - " + "Key is: "
						+ key + " value is: " + rawXMLData);

				try {
					facadeInformationReader = FacadeInformationReader
							.parseFromXMLData(rawXMLData);
					facadeServicesInformation = facadeInformationReader
							.getFacadeServicesInformation();
					facadeConfiguration = facadeServicesInformation
							.getFacadeConfigurationList().get(0);
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
				}

			} else {
				// export_asp1_ta_2
				// asp1_ta_2.xml
				if (key
						.startsWith(XmlConstants.ASPECTPOLICYGROUP_EXPORT_ASP_STARTSWITH_KEY)) {
					value = entry.getValue()[0];
					System.out.println("***** In else block - " + "Key is: "
							+ key + " value is: " + value);
					String fileName = key.replaceAll(
							XmlConstants.ASPECTPOLICYGROUP_EXPORT_KEY, "")
							+ XMLEXTENSION_KEY;
					nameToConfigMap.put(fileName, value);
					System.out.println("FileName: " + fileName);
					System.out.println(value);
					try {
						Advice advice = null;
                                                if((key != null) && (value != null)) {
                                                    advice = AdviceReader.getAdvice(value);
                                                    policyGroup.addToAdviceString(key, advice
                                                                    .getAdviceConfiguration());
                                                }
					} catch (ParserConfigurationException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (SAXException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (URISyntaxException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

				} else {
					System.out.println("***** In else/else block - "
							+ "Key is: " + key + " value is: " + value);
				}
			}
		}
		String strippedPolicyGroupName = StringHelper
				.stripWhitespace(policyGroupName);
		if ((strippedPolicyGroupName == null)
				|| (strippedPolicyGroupName.trim().length() < 1)) {
			System.out.println("Service Assembly " + policyGroupName
					+ " when stripped of whitespace is invalid...");
			System.out.println("Redirecting to " + POLICYGROUPS_URL);
			response.sendRedirect(POLICYGROUPS_URL);
		}
		policyGroup.setName(strippedPolicyGroupName);

		// ///////////////////////////////////////////////////////
		// 1. Create project structure in storage
		// ///////////////////////////////////////////////////////
		// ..workspace
		// ...+-catalog.xml
		// ...+-catalog
		// ...| +-01000000-4D391F5C110100-8199A797-01
		// ...| | +-servicewsdl.wsdl
		// ...| | +-original.xsd
		// ...| +-01000000-FB3D205C110100-8199A797-01
		// ...| +-servicewsdl2.wsdl
		// ...| +-servicewsdl3.wsdl
		// ...| +-original.xsd
		// ...| +-original4.xsd
		// ...+-aspectpolicygroups
		// ......+-PolicyGroupName
		// .........+-src
		// .........| +-AspectPolicyUnit
		// .........| | +-src
		// .........| | | +-facadewsdl.wsdl
		// .........| | | +-original.xsd
		// .........| | | +-aspectmap.xml
		// .........| | | +-configuration1.xml
		// .........| | | +-configuration2.xml
		// .........| | +-build
		// .........| | | +-META-INF
		// .........| | | +-jbi.xml
		// .........| | | +-Manifest.mf
		// .........| | +-dist
		// .........| | | +-AspectPolicyUnit.jar
		// .........| +-sun-http-binding
		// .........| | +-src
		// .........| | | +-facadewsdl.wsdl
		// .........| | | +-original.xsd
		// .........| | +-build
		// .........| | | +-META-INF
		// .........| | | +-jbi.xml
		// .........| | | +-Manifest.mf
		// .........| | +-dist
		// .........| | | +-sun-http-binding.jar
		// .........+-build
		// .........| +-META-INF
		// .........| | +-jbi.xml
		// .........| | +-Manifest.mf
		// .........| +-AspectPolicyUnit.jar
		// .........| +-sun-http-binding.jar
		// .........+-dist
		// .........| +-PolicyGroupName.zip
		// ///////////////////////////////////////////////
		String workspacePath = rootPath + File.separator
				+ WORKSPACE_FOLDERNAME_KEY;
		File workspaceFolder = FileUtilities.createFolder(workspacePath);
		String catalogPath = workspacePath + File.separator
				+ CATALOG_FOLDERNAME_KEY;
		File catalogFolder = FileUtilities.createFolder(catalogPath);

		String aspectpolicygroupsPath = workspacePath + File.separator
				+ ASPECTPOLICYGROUPS_FOLDERNAME_KEY;
		File aspectpolicygroupsFolder = FileUtilities
				.createFolder(aspectpolicygroupsPath);
		policyGroupCollection.setBaseLocation(aspectpolicygroupsFolder);

		String policyGroupNamePath = aspectpolicygroupsPath + File.separator
				+ strippedPolicyGroupName;
		File policyGroupNameFolder = FileUtilities
				.createFolder(policyGroupNamePath);
		policyGroup.setBaseLocation(policyGroupNameFolder);

		// Service Assembly folder structure
		String saSrcPath = policyGroupNamePath + File.separator
				+ SRC_FOLDERNAME_KEY;
		File saSrcFolder = FileUtilities.createFolder(saSrcPath);
		String saBuildPath = policyGroupNamePath + File.separator
				+ BUILD_FOLDERNAME_KEY;
		File saBuildFolder = FileUtilities.createFolder(saBuildPath);
		String saBuildMetaInfPath = saBuildPath + File.separator
				+ META_INF_FOLDERNAME_KEY;
		File saBuildMetaInfFolder = FileUtilities
				.createFolder(saBuildMetaInfPath);
		String saDistPath = policyGroupNamePath + File.separator
				+ DIST_FOLDERNAME_KEY;
		File saDistFolder = FileUtilities.createFolder(saDistPath);

		String aspectUnitPath = saSrcPath + File.separator
				+ ASPECTSERVICEUNITNAME_KEY;
		File aspectUnitFolder = FileUtilities.createFolder(aspectUnitPath);
		// Aspect Service Unit folder structure
		String aspectUnitSrcPath = aspectUnitPath + File.separator
				+ SRC_FOLDERNAME_KEY;
		File aspectUnitSrcFolder = FileUtilities
				.createFolder(aspectUnitSrcPath);
		String aspectUnitBuildPath = aspectUnitPath + File.separator
				+ BUILD_FOLDERNAME_KEY;
		File aspectUnitBuildFolder = FileUtilities
				.createFolder(aspectUnitBuildPath);
		String aspectUnitBuildMetaInfPath = aspectUnitBuildPath
				+ File.separator + META_INF_FOLDERNAME_KEY;
		File aspectUnitBuildMetaInfFolder = FileUtilities
				.createFolder(aspectUnitBuildMetaInfPath);
		String aspectUnitDistPath = aspectUnitPath + File.separator
				+ DIST_FOLDERNAME_KEY;
		File aspectUnitDistFolder = FileUtilities
				.createFolder(aspectUnitDistPath);

		String bindingUnitPath = saSrcPath + File.separator
				+ HTTPSERVICEUNITNAME_KEY;
		File bindingUnitFolder = FileUtilities.createFolder(bindingUnitPath);
		// Binding Service Unit folder structure
		String bindingUnitSrcPath = bindingUnitPath + File.separator
				+ SRC_FOLDERNAME_KEY;
		File bindingUnitSrcFolder = FileUtilities
				.createFolder(bindingUnitSrcPath);
		String bindingUnitBuildPath = bindingUnitPath + File.separator
				+ BUILD_FOLDERNAME_KEY;
		File bindingUnitBuildFolder = FileUtilities
				.createFolder(bindingUnitBuildPath);
		String bindingUnitBuildMetaInfPath = bindingUnitBuildPath
				+ File.separator + META_INF_FOLDERNAME_KEY;
		File bindingUnitBuildMetaInfFolder = FileUtilities
				.createFolder(bindingUnitBuildMetaInfPath);
		String bindingUnitDistPath = bindingUnitPath + File.separator
				+ DIST_FOLDERNAME_KEY;
		File bindingUnitDistFolder = FileUtilities
				.createFolder(bindingUnitDistPath);

		// ///////////////////////////////////////////////////////
		// 2. Copy adviceConfiguration files into the aspect
		// unit area in storage
		// ///////////////////////////////////////////////////////
		for (Entry<String /* fileName */, String /* configString */> entry : nameToConfigMap
				.entrySet()) {
			String fileName = entry.getKey();
			String fileContent = entry.getValue();
			File file = new File(aspectUnitSrcPath, fileName);
			try {
				file.createNewFile();
				System.out.println("Adding file: " + file.getAbsolutePath());
				System.out.println("Adding value: " + fileContent);
				FileUtilities.writeToFile(file.getAbsolutePath(), fileContent);
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (UnsupportedEncodingException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (SyncFailedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		// ///////////////////////////////////////////////////////
		// 3. Copy WSDLs and XSD into the aspect and binding
		// service unit areas in storage
		// ///////////////////////////////////////////////////////
		for (ProviderConfiguration providerConfig : providerServicesInformation
				.getProviderConfigurationList()) {
			if ((providerConfig.getServiceQName() != null)
					&& (providerConfig.getPortName() != null)
					&& (providerConfig.getPortTypeQName() != null)) {
				providerConfiguration = providerConfig;
				break;
			}
		}
		QName qName = providerConfiguration.getServiceQName();
		File wsdlFile = null;
		File catalogFile = new File(workspaceFolder,
				CatalogWriter.FILE_NAME_KEY);
		CatalogReader parser = null;
		Catalog catalog = null;
		Map<QName /* ServiceQName */, String /* URI */> serviceQNameToURIListMap = null;
		serviceQNameToURIListMap = new HashMap<QName /* ServiceQName */, String /* URI */>();
		try {
			parser = CatalogReader.parseFromFile(catalogFile.getAbsolutePath());
		} catch (Exception exception) {
		}
		if (parser != null) {
			catalog = parser.getCatalog();
			if (catalog != null) {
				List<WSDLService> wsdlServiceList = Catalog
						.findWSDLServiceList(catalog, qName.toString());
				for (WSDLService service : wsdlServiceList) {
					if ((service.getServiceWSDLFile() == null)
							|| (service.getFolder() == null)
							|| (service.getLocationURI() == null)
							|| (service.getName() == null)
							|| (service.getPortName() == null)
							|| (service.getPortType() == null)
							|| (service.getServiceGroupName() == null)
							|| (service.getTargetNamespace() == null)) {
						continue;
					}
					List<File> wsdlFiles = service.getWsdlFiles();
					for (File file : wsdlFiles) {
						if (file.getName().equals(
								service.getServiceWSDLFile().getName())) {
							continue;
						}
						String contents = ReadWriteTextFile.getContents(file);
						File aspectOutputFile = new File(aspectUnitSrcPath,
								file.getName());
						File bindingOutputFile = new File(bindingUnitSrcPath,
								file.getName());

						try {
							aspectOutputFile.createNewFile();
							FileUtilities.writeToFile(aspectOutputFile
									.getAbsolutePath(), contents);
							bindingOutputFile.createNewFile();
							FileUtilities.writeToFile(bindingOutputFile
									.getAbsolutePath(), contents);
						} catch (FileNotFoundException e2) {
							// TODO Auto-generated catch block
							e2.printStackTrace();
						} catch (UnsupportedEncodingException e2) {
							// TODO Auto-generated catch block
							e2.printStackTrace();
						} catch (SyncFailedException e2) {
							// TODO Auto-generated catch block
							e2.printStackTrace();
						} catch (IOException e2) {
							// TODO Auto-generated catch block
							e2.printStackTrace();
						}
					}
					List<File> xsdFiles = service.getXsdFiles();
					for (File file : xsdFiles) {
						String contents = ReadWriteTextFile.getContents(file);
						File aspectOutputFile = new File(aspectUnitSrcPath,
								file.getName());
						File bindingOutputFile = new File(bindingUnitSrcPath,
								file.getName());

						try {
							aspectOutputFile.createNewFile();
							FileUtilities.writeToFile(aspectOutputFile
									.getAbsolutePath(), contents);
							bindingOutputFile.createNewFile();
							FileUtilities.writeToFile(bindingOutputFile
									.getAbsolutePath(), contents);
						} catch (FileNotFoundException e2) {
							// TODO Auto-generated catch block
							e2.printStackTrace();
						} catch (UnsupportedEncodingException e2) {
							// TODO Auto-generated catch block
							e2.printStackTrace();
						} catch (SyncFailedException e2) {
							// TODO Auto-generated catch block
							e2.printStackTrace();
						} catch (IOException e2) {
							// TODO Auto-generated catch block
							e2.printStackTrace();
						}
					}
					wsdlFile = service.getServiceWSDLFile();
					if ((qName != null) && (wsdlFile != null)) {
						serviceQNameToURIListMap.put(qName, wsdlFile
								.getAbsolutePath());
					}
				}
			}
		}

		// ///////////////////////////////////////////////////////
		// 4. Read the original WSDL and Generate Facade WSDL
		// ///////////////////////////////////////////////////////
		String originalUri = serviceQNameToURIListMap.get(providerConfiguration
				.getServiceQName());
		WSDLModel wsdlModel = new WSDLModelHelper();
		FacadeWSDLCreator facadeWsdlCreator = new FacadeWSDLCreator();
		String partnerLinkTypeName = providerConfiguration.getServiceQName()
				.getLocalPart()
				+ "PartnerLinkType";
		QName partnerLinkQName = new QName(providerConfiguration
				.getServiceQName().getNamespaceURI(), providerConfiguration
				.getServiceQName().getLocalPart()
				+ "PartnerLink", providerConfiguration.getServiceQName()
				.getPrefix());
		String roleName = providerConfiguration.getPortName() + "RoleName";
		QName portTypeQName = providerConfiguration.getPortTypeQName();
		PartnerConfiguration partnerConfiguration = new PartnerConfiguration(
				partnerLinkTypeName, partnerLinkQName, roleName, portTypeQName);
		File facadeWSDLOutputFile = new File(aspectUnitSrcPath,
				"facadewsdl.wsdl");

		try {
			try {
				wsdlModel.populate(originalUri);
			} catch (XmlException ex) {
				// ex.printStackTrace();
				System.out.println(ex.getMessage());
			}
			facadeConfiguration.setTargetNamespace(providerConfiguration
					.getServiceQName().getNamespaceURI());
			if ((facadeConfiguration.getFacadeServiceName() == null)
					|| (facadeConfiguration.getFacadeServiceName().trim()
							.length() <= 1)) {
				facadeConfiguration.setFacadeServiceName("facade"
						+ providerConfiguration.getServiceQName()
								.getLocalPart());

			}
			if ((facadeConfiguration.getFacadePortName() == null)
					|| (facadeConfiguration.getFacadePortName().trim().length() <= 1)) {
				facadeConfiguration.setFacadePortName("facade"
						+ providerConfiguration.getPortName());

			}
			if ((facadeConfiguration.getLocationURI() == null)
					|| (facadeConfiguration.getLocationURI().trim().length() <= 1)) {
				facadeConfiguration
						.setLocationURI("http://localhost:28000/facade"
								+ providerConfiguration.getServiceQName()
										.getLocalPart());

			}
			if (facadeConfiguration.getPortTypeQName() == null) {
				facadeConfiguration.setPortTypeQName(providerConfiguration
						.getPortTypeQName());
			}
			policyGroup.addToInputString(workspaceFolder, providerConfiguration);
			policyGroup.addToFacadeString(facadeConfiguration);
			wsdlModel = facadeWsdlCreator.addToWSDL(wsdlModel,
					facadeWSDLOutputFile, providerConfiguration,
					partnerConfiguration, facadeConfiguration);
		} catch (MalformedURLException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (TransformerConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (WSDLException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (XmlException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (SAXException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (ParserConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (TransformerException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}

		// ///////////////////////////////////////////////////////
		// 5. Copy Facade WSDL and XSD into the
		// HTTP BC service unit area in storage
		// ///////////////////////////////////////////////////////
		String contents = ReadWriteTextFile.getContents(facadeWSDLOutputFile);
		File facadeWSDLBindingOutputFile = new File(bindingUnitSrcPath,
				"facadewsdl.wsdl");
		try {
			facadeWSDLBindingOutputFile.createNewFile();
			FileUtilities.writeToFile(facadeWSDLBindingOutputFile
					.getAbsolutePath(), contents);
		} catch (FileNotFoundException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		} catch (UnsupportedEncodingException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		} catch (SyncFailedException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		} catch (IOException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		}

		// ///////////////////////////////////////////////////////
		// 6. Create the Real Aspect Map in memory
		// ///////////////////////////////////////////////////////
		AspectMapCreator creator = new AspectMapCreator();
		AspectMap aspectMap = creator.generateAspectMap(wsdlModel,
				providerConfiguration, partnerConfiguration, adviceList);

		// ///////////////////////////////////////////////////////
		// 7. Write Aspect Map to aspect unit area in storage
		// ///////////////////////////////////////////////////////
		AspectMapWriter serializer = new AspectMapWriter();

		try {
			String xmlString = serializer.serialize(aspectMap);
			File file = new File(aspectUnitSrcPath, "aspectmap.xml");
			file.createNewFile();
			FileUtilities.writeToFile(file.getAbsolutePath(), xmlString);
		} catch (FileNotFoundException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		} catch (UnsupportedEncodingException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		} catch (SyncFailedException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		} catch (ParserConfigurationException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		} catch (TransformerException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		} catch (IOException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		}

		// ///////////////////////////////////////////////////////
		// 8. Create AspectMap service unit jbi.xml in META-INF
		// folder of the aspect service unit area in storage
		// ///////////////////////////////////////////////////////
		EngineServiceUnitDescriptorGenerator aspectJbiGenerator = new EngineServiceUnitDescriptorGenerator();

		String content;
		String outputFileLocation;
		try {
			content = aspectJbiGenerator.serialize(aspectMap);
			System.out.println(content);
			outputFileLocation = aspectUnitBuildMetaInfPath + File.separator
					+ "jbi.xml";
			aspectJbiGenerator.writeToFile(content, outputFileLocation);
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (UnsupportedEncodingException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (ParserConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (TransformerException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}

		// ///////////////////////////////////////////////////////
		// 9. Package AspectMap service unit
		// ///////////////////////////////////////////////////////
		for (File source : aspectUnitSrcFolder.listFiles()) {
			if (source != null) {
				File target = new File(aspectUnitBuildPath, source.getName());
				try {
					FileUtilities.copyFile(source, target);
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		List<File> listOfFilesToBeJared = new ArrayList<File>();
		for (File source : aspectUnitBuildFolder.listFiles()) {
			if (source != null) {
				if (source.isDirectory() == true) {
					for (File sourceElement : source.listFiles()) {
						if (sourceElement != null) {
							listOfFilesToBeJared.add(sourceElement);
						}
					}
				} else {
					listOfFilesToBeJared.add(source);
				}
			}
		}
		File aspectServiceUnitJarFile = new File(aspectUnitDistPath,
				ASPECTSERVICEUNITNAME_KEY + ".jar");
		JarFileUtility.createArchive(aspectServiceUnitJarFile,
				listOfFilesToBeJared);

		// ///////////////////////////////////////////////////////
		// 10. Create HTTP BC service unit jbi.xml in META-INF
		// folder of the aspect service unit area in storage
		// ///////////////////////////////////////////////////////
		BindingServiceUnitDescriptorGenerator httpJbiGenerator = new BindingServiceUnitDescriptorGenerator();

		try {
			facadeConfiguration.setTargetNamespace(providerConfiguration
					.getServiceQName().getNamespaceURI());
			if ((facadeConfiguration.getFacadeServiceName() == null)
					|| (facadeConfiguration.getFacadeServiceName().trim()
							.length() <= 1)) {
				facadeConfiguration.setFacadeServiceName("facade"
						+ providerConfiguration.getServiceQName()
								.getLocalPart());

			}
			if ((facadeConfiguration.getFacadePortName() == null)
					|| (facadeConfiguration.getFacadePortName().trim().length() <= 1)) {
				facadeConfiguration.setFacadePortName("facade"
						+ providerConfiguration.getPortName());

			}
			if ((facadeConfiguration.getLocationURI() == null)
					|| (facadeConfiguration.getLocationURI().trim().length() <= 1)) {
				facadeConfiguration
						.setLocationURI("http://localhost:28000/facade"
								+ providerConfiguration.getServiceQName()
										.getLocalPart());

			}
			if (facadeConfiguration.getPortTypeQName() == null) {
				facadeConfiguration.setPortTypeQName(providerConfiguration
						.getPortTypeQName());
			}
			content = httpJbiGenerator
					.serialize(aspectMap, facadeConfiguration);
			System.out.println(content);
			outputFileLocation = bindingUnitBuildMetaInfPath + File.separator
					+ "jbi.xml";
			httpJbiGenerator.writeToFile(content, outputFileLocation);
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ParserConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TransformerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		// ///////////////////////////////////////////////////////
		// 11. Package the HTTP BC service unit
		// ///////////////////////////////////////////////////////
		for (File source : bindingUnitSrcFolder.listFiles()) {
			if (source != null) {
				File target = new File(bindingUnitBuildPath, source.getName());
				try {
					FileUtilities.copyFile(source, target);
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		listOfFilesToBeJared = new ArrayList<File>();
		for (File source : bindingUnitBuildFolder.listFiles()) {
			if (source != null) {
				if (source.isDirectory() == true) {
					for (File sourceElement : source.listFiles()) {
						if (sourceElement != null) {
							listOfFilesToBeJared.add(sourceElement);
						}
					}
				} else {
					listOfFilesToBeJared.add(source);
				}
			}
		}
		File bindingServiceUnitJarFile = new File(bindingUnitDistPath,
				HTTPSERVICEUNITNAME_KEY + ".jar");
		JarFileUtility.createArchive(bindingServiceUnitJarFile,
				listOfFilesToBeJared);

		// ///////////////////////////////////////////////////////
		// 12. Create Service Assembly jbi.xml in META-INF
		// folder of the service assembly area in storage
		// ///////////////////////////////////////////////////////
		try {
			outputFileLocation = saBuildMetaInfPath + File.separator
					+ "jbi.xml";
			ServiceAssemblyDescriptorGenerator assemblyDescriptor = new ServiceAssemblyDescriptorGenerator();
			facadeConfiguration.setTargetNamespace(providerConfiguration
					.getServiceQName().getNamespaceURI());
			if ((facadeConfiguration.getFacadeServiceName() == null)
					|| (facadeConfiguration.getFacadeServiceName().trim()
							.length() <= 1)) {
				facadeConfiguration.setFacadeServiceName("facade"
						+ providerConfiguration.getServiceQName()
								.getLocalPart());

			}
			if ((facadeConfiguration.getFacadePortName() == null)
					|| (facadeConfiguration.getFacadePortName().trim().length() <= 1)) {
				facadeConfiguration.setFacadePortName("facade"
						+ providerConfiguration.getPortName());

			}
			if ((facadeConfiguration.getLocationURI() == null)
					|| (facadeConfiguration.getLocationURI().trim().length() <= 1)) {
				facadeConfiguration
						.setLocationURI("http://localhost:28000/facade"
								+ providerConfiguration.getServiceQName()
										.getLocalPart());

			}
			if (facadeConfiguration.getPortTypeQName() == null) {
				facadeConfiguration.setPortTypeQName(providerConfiguration
						.getPortTypeQName());
			}
			content = assemblyDescriptor.serialize(strippedPolicyGroupName,
					"Aspect Policy Group for " + policyGroupName, aspectMap,
					facadeConfiguration);
			System.out.println(content);
			assemblyDescriptor.writeToFile(content, outputFileLocation);
		} catch (ParserConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TransformerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		// ///////////////////////////////////////////////////////
		// 13. Package the service assembly
		// ///////////////////////////////////////////////////////
		File builtBindingUnitJarFile = new File(saBuildFolder,
				HTTPSERVICEUNITNAME_KEY + ".jar");
		FileUtilities.copyFile(bindingServiceUnitJarFile,
				builtBindingUnitJarFile);
		File builtAspectUnitJarFile = new File(saBuildFolder,
				ASPECTSERVICEUNITNAME_KEY + ".jar");
		FileUtilities
				.copyFile(aspectServiceUnitJarFile, builtAspectUnitJarFile);

		listOfFilesToBeJared = new ArrayList<File>();
		for (File source : saBuildFolder.listFiles()) {
			if (source != null) {
				if (source.isDirectory() == true) {
					for (File sourceElement : source.listFiles()) {
						if (sourceElement != null) {
							listOfFilesToBeJared.add(sourceElement);
						}
					}
				} else {
					listOfFilesToBeJared.add(source);
				}
			}
		}
		File serviceAssemblyZipFile = new File(saDistPath,
				strippedPolicyGroupName + ".zip");
		JarFileUtility.createArchive(serviceAssemblyZipFile,
				listOfFilesToBeJared);

		// ///////////////////////////////////////////////////////
		// 14. Deploy the service assembly
		// ///////////////////////////////////////////////////////
		JBIAdminCommands commands = null;
		MBeanServer connection = this.getLocalServerConnector();
		if (connection != null) {
			try {
				commands = JBIAdminCommandsClientFactory
						.getInstance(connection);
			} catch (JBIRemoteException e) {
				e.printStackTrace();
			}
			if (commands != null) {
				// Stop, Shutdown undeploy existing
				try {
					commands.stopComponent(strippedPolicyGroupName,
							JBIAdminCommands.SERVER_TARGET_KEY);
					commands.shutdownComponent(strippedPolicyGroupName,
							JBIAdminCommands.SERVER_TARGET_KEY);
					commands.undeployServiceAssembly(strippedPolicyGroupName,
							JBIAdminCommands.SERVER_TARGET_KEY);
				} catch (JBIRemoteException e) {
					e.printStackTrace();
				}
				// Deploy and start new Policy Group
				try {
                                        File file = new File(workspaceFolder, AspectPolicyGroupWriter.FILE_NAME_KEY);
                                        if(file.exists() == true) {
                                            try {
                                                policyGroupCollection = AspectPolicyGroupReader.getPolicyGroupCollection(file);
                                            } catch (MalformedURLException ex) {
                                                ex.printStackTrace();
                                            } catch (URISyntaxException ex) {
                                                ex.printStackTrace();
                                            } catch (ParserConfigurationException ex) {
                                                ex.printStackTrace();
                                            } catch (SAXException ex) {
                                                ex.printStackTrace();
                                            } catch (IOException ex) {
                                                ex.printStackTrace();
                                            }
                                        } else {
                                            file.createNewFile();
                                        }
					policyGroupCollection.addPolicyGroup(policyGroup);
					AspectPolicyGroupWriter aspectPolicyGroupWriter = new AspectPolicyGroupWriter();
					try {
						String xmlString = aspectPolicyGroupWriter.serialize(policyGroupCollection);
						FileUtilities.writeToFile(file.getAbsolutePath(), xmlString);
					} catch (FileNotFoundException e2) {
						// TODO Auto-generated catch block
						e2.printStackTrace();
					} catch (UnsupportedEncodingException e2) {
						// TODO Auto-generated catch block
						e2.printStackTrace();
					} catch (SyncFailedException e2) {
						// TODO Auto-generated catch block
						e2.printStackTrace();
					} catch (ParserConfigurationException e2) {
						// TODO Auto-generated catch block
						e2.printStackTrace();
					} catch (TransformerException e2) {
						// TODO Auto-generated catch block
						e2.printStackTrace();
					} catch (IOException e2) {
						// TODO Auto-generated catch block
						e2.printStackTrace();
					}					
					
					commands.deployServiceAssembly(serviceAssemblyZipFile
							.getAbsolutePath(),
							JBIAdminCommands.SERVER_TARGET_KEY);
					commands.startServiceAssembly(strippedPolicyGroupName,
							JBIAdminCommands.SERVER_TARGET_KEY);
				} catch (JBIRemoteException e) {
					e.printStackTrace();
				}
			}
		}

		// ///////////////////////////////////////////////////////
		// 15. Forward request to the policy groups page
		// ///////////////////////////////////////////////////////
		RequestDispatcher dispatcher = context
				.getRequestDispatcher(POLICYGROUPS_URL);
		dispatcher.forward(request, response);
	}

	/**
	 * Get an MBeanServer connection
	 * 
	 * @return
	 */
	private MBeanServer getLocalServerConnector() {
		return (MBeanServer) MBeanServerFactory.findMBeanServer(null).get(0);
	}

	// <editor-fold defaultstate="collapsed" desc="HttpServlet methods. Click on
	// the + sign on the left to edit the code.">
	/**
	 * Handles the HTTP <code>GET</code> method.
	 * 
	 * @param request
	 *            servlet request
	 * @param response
	 *            servlet response
	 */
	protected void doGet(HttpServletRequest request,
			HttpServletResponse response) throws ServletException, IOException {
		processRequest(request, response);
	}

	/**
	 * Handles the HTTP <code>POST</code> method.
	 * 
	 * @param request
	 *            servlet request
	 * @param response
	 *            servlet response
	 */
	protected void doPost(HttpServletRequest request,
			HttpServletResponse response) throws ServletException, IOException {
		processRequest(request, response);
	}

	/**
	 * Returns a short description of the servlet.
	 */
	public String getServletInfo() {
		return "Aspects Save Action";
	}
	// </editor-fold>
}
