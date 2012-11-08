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
 * @(#)Catalog.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.model.catalog;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;

import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;

import org.apache.xmlbeans.XmlException;
import org.xml.sax.SAXException;

import com.sun.jbi.cam.common.FileUtilities;
import com.sun.jbi.cam.common.ReadWriteTextFile;
import com.sun.jbi.cam.common.UUIDGenerator;
import com.sun.jbi.cam.common.ZipFileUtility;
import com.sun.jbi.cam.plugins.aspects.common.AspectsGenericConstants;
import com.sun.jbi.cam.plugins.aspects.common.XmlConstants;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.xml.CatalogReader;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.xml.CatalogWriter;
import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.WSDLModel;
import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.WSDLModelHelper;

/**
 * @author graj
 * 
 */
public class Catalog implements Serializable {
	private static final long serialVersionUID = 1L;

	Map<String /*baseLocationString*/, WebServices> servicesMap = new HashMap<String, WebServices>();

	// List<WebServices> servicesList = new ArrayList<WebServices>();

	/**
	 * 
	 */
	public Catalog() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @return the servicesList
	 */
	public List<WebServices> getWebServicesList() {
		List<WebServices> servicesList = new ArrayList<WebServices>();
		for (WebServices webServices : this.servicesMap.values()) {
			servicesList.add(webServices);
		}
		return servicesList;
	}

	/**
	 * @param servicesList
	 *            the servicesList to set
	 */
	public void setWebServicesList(List<WebServices> servicesList) {
		for (WebServices webServices : servicesList) {
			this.servicesMap.put(webServices.getBaseLocation()
					.getAbsolutePath(), webServices);
		}
	}

	/**
	 * @param webServices
	 *            the webServices to add
	 */
	public void addWebServicesToCatalog(WebServices webServices) {
		this.servicesMap.put(webServices.getBaseLocation().getAbsolutePath(),
				webServices);
	}

	/**
	 * @param webServices
	 *            the webServices to remove
	 */
	public void removeWebServicesFromCatalog(WebServices webServices) {
		this.servicesMap
				.remove(webServices.getBaseLocation().getAbsolutePath());
	}

	/**
	 * 
	 * @param baseLocation
	 * @return webServices the webServices object
	 */
	public WebServices getWebServices(String baseLocation) {
		return this.servicesMap.get(baseLocation);
	}

	/**
	 * 
	 * @param workspaceFolder
	 * @return
	 */
	public static File getCatalogFolder(File workspaceFolder) {
		File catalogFolder = new File(workspaceFolder,
				XmlConstants.CATALOG_CATALOG_KEY);
		if (catalogFolder.exists() == false) {
			FileUtilities.createFolder(catalogFolder.getAbsolutePath());
		}
		return catalogFolder;
	}

	/**
	 * 
	 * @param workspaceFolder
	 */
	public static void createWorkspaceFolder(File workspaceFolder) {
		if (workspaceFolder.exists() == false) {
			FileUtilities.createFolder(workspaceFolder.getAbsolutePath());
		}
	}

	/**
	 * 
	 * @param catalogFile
	 * @param catalog
	 * @param workspaceFolder
	 * @return
	 * @throws MalformedURLException
	 * @throws ParserConfigurationException
	 * @throws SAXException
	 * @throws IOException
	 * @throws URISyntaxException
	 */
	public static Catalog getCatalog(File catalogFile, Catalog catalog,
			File workspaceFolder) throws MalformedURLException,
			ParserConfigurationException, SAXException, IOException,
			URISyntaxException {
		if (catalogFile.exists() == true) {
			CatalogReader parser = CatalogReader.parseFromFile(catalogFile
					.getAbsolutePath());
			catalog = parser.getCatalog();
		} else {
			catalogFile.createNewFile();
		}
		if (catalog == null) {
			// create a new one
			catalog = new Catalog();
		}
		return catalog;
	}

	/**
	 * 
	 * @param workspaceFolder
	 * @param wsdlURI
	 * @return
	 * @throws WSDLException
	 * @throws XmlException
	 * @throws MalformedURLException
	 * @throws ParserConfigurationException
	 * @throws SAXException
	 * @throws URISyntaxException
	 * @throws TransformerException
	 * @throws IOException
	 */
	public static List<WSDLService> createServiceEntries(
			String serviceGroupName, File workspaceFolder, String wsdlURI)
			throws WSDLException, MalformedURLException,
			ParserConfigurationException, SAXException, URISyntaxException,
			TransformerException, IOException {

		List<WSDLService> wsdlServiceList = new ArrayList<WSDLService>();
		WebServices webServices = null;
		WSDLService wsdlService = null;
		Catalog catalog = null;

		createWorkspaceFolder(workspaceFolder);
		File catalogFolder = getCatalogFolder(workspaceFolder);

		CatalogReader parser = null;
		File catalogFile = new File(workspaceFolder,
				CatalogWriter.FILE_NAME_KEY);
		catalog = getCatalog(catalogFile, catalog, workspaceFolder);

		if (catalog != null) {
			webServices = catalog.getWebServices(workspaceFolder
					.getAbsolutePath());
		}

		if (webServices == null) {
			webServices = new WebServices(workspaceFolder);
		}

		// ///////////////////////////////////////
		// Store the WSDL file in a unique folder
		// ///////////////////////////////////////
		List<File> wsdlFileList = new ArrayList<File>();
		List<File> xsdFileList = new ArrayList<File>();
		String uniqueID = null;
		String uniqueFolderString = null;
		File uniqueFolder = null;
		WSDLModel wsdlModel = new WSDLModelHelper();
		try {
			wsdlModel.populate(wsdlURI);
		} catch (XmlException e) {
			//e.printStackTrace();
			System.out.println("Exception: " + e.getMessage());
		}
		Definition definition = wsdlModel.getDefinition();
		Map<QName /* serviceQName */, Service> servicesMap = definition
				.getServices();
		for (QName serviceQName : servicesMap.keySet()) {
			File folder = webServices.getUniqueFolder(serviceQName);
			if ((folder != null) && (folder.exists() == true)) {
				uniqueFolderString = folder.getAbsolutePath();
				uniqueFolder = folder;
				uniqueID = folder.getName();
				break;
			}
		}
		String rawXmlData = wsdlModel.getWSDLString(wsdlURI);
		if (uniqueFolderString == null) {
			uniqueID = UUIDGenerator.getNUID();
			uniqueFolderString = workspaceFolder.getAbsolutePath()
					+ File.separator + XmlConstants.CATALOG_CATALOG_KEY
					+ File.separator + uniqueID;
		}
		if ((uniqueFolder == null) || (uniqueFolder.exists() == false)) {
			uniqueFolder = FileUtilities.createFolder(uniqueFolderString);
		}
		File wsdlFile = new File(uniqueFolder, uniqueID
				+ AspectsGenericConstants.WSDL_SUFFIX);
		FileUtilities.writeToFile(wsdlFile, rawXmlData);
		wsdlFileList.add(wsdlFile);

		// ///////////////////////////////////////
		// For each Service, create equivalent
		// WSDLService objects and add
		// them to the Catalog for each Service
		// ///////////////////////////////////////
		Service service = null;
		Port port = null;
		PortType portType = null;
		for (QName serviceQName : servicesMap.keySet()) {
			String targetNamespace = serviceQName.getNamespaceURI();
			service = servicesMap.get(serviceQName);
			Map<String, Port> portsMap = service.getPorts();
			for (String portName : portsMap.keySet()) {
				port = portsMap.get(portName);
				String locationURI = wsdlModel.getSOAPLocationURI(port);
				if (locationURI == null) {
					// Skip since there is no
					// SOAP location URI
					continue;
				}
				portType = port.getBinding().getPortType();
				wsdlService = new WSDLService(serviceQName, serviceGroupName,
						targetNamespace, portName, locationURI, portType
								.getQName(), uniqueFolder, wsdlFile,
						wsdlFileList, xsdFileList);
				if (null != webServices.addWsdlServiceToList(wsdlService)) {
					wsdlServiceList.add(wsdlService);
				}
			}
		}

		catalog.addWebServicesToCatalog(webServices);
		catalogWrite(catalog, catalogFile);

		return wsdlServiceList;
	}

	/**
	 * 
	 * @param serviceGroupName
	 * @param workspaceFolder
	 * @param wsdlXsdFileArray
	 * @return
	 * @throws WSDLException
	 * @throws MalformedURLException
	 * @throws ParserConfigurationException
	 * @throws SAXException
	 * @throws URISyntaxException
	 * @throws TransformerException
	 * @throws IOException
	 */
	public static List<WSDLService> createServiceEntries(
			String serviceGroupName, File workspaceFolder, File uniqueFolder,
			File[] wsdlXsdFileArray) throws WSDLException,
			MalformedURLException, ParserConfigurationException, SAXException,
			URISyntaxException, TransformerException, IOException {
		return createServiceEntries(serviceGroupName, workspaceFolder,
				uniqueFolder, wsdlXsdFileArray, true);
	}

	private static File getUniqueFolder(File uniqueFolder,
			File workspaceFolder, boolean create) {
		String uniqueID = null;
		String uniqueFolderString = null;
		if (create) {
			uniqueID = UUIDGenerator.getNUID();
			uniqueFolderString = workspaceFolder.getAbsolutePath()
					+ File.separator + XmlConstants.CATALOG_CATALOG_KEY
					+ File.separator + uniqueID;
			uniqueFolder = FileUtilities.createFolder(uniqueFolderString);
		}
		return uniqueFolder;
	}

	private static void storeFiles(File[] wsdlXsdFileArray,
			List<File> wsdlFileList, List<File> xsdFileList, File uniqueFolder,
			boolean create) throws IOException {

		String rawXmlData = null;
		for (File aFile : wsdlXsdFileArray) {
			if (aFile.getName().endsWith(AspectsGenericConstants.XSD_SUFFIX)) {
				xsdFileList.add(aFile);
				if (create) {
					rawXmlData = ReadWriteTextFile.getContents(aFile);
					File newFile = new File(uniqueFolder, aFile.getName());
					newFile.createNewFile();
					FileUtilities.writeToFile(newFile, rawXmlData);
				}
			}
			if (aFile.getName().endsWith(AspectsGenericConstants.WSDL_SUFFIX)) {
				wsdlFileList.add(aFile);
				if (create) {
					rawXmlData = ReadWriteTextFile.getContents(aFile);
					File newFile = new File(uniqueFolder, aFile.getName());
					newFile.createNewFile();
					FileUtilities.writeToFile(newFile, rawXmlData);
				}
			}
		}

	}

	/**
	 * 
	 * @param serviceGroupName
	 * @param workspaceFolder
	 * @param wsdlXsdFileArray
	 * @param create
	 * @return
	 * @throws WSDLException
	 * @throws MalformedURLException
	 * @throws ParserConfigurationException
	 * @throws SAXException
	 * @throws URISyntaxException
	 * @throws TransformerException
	 * @throws IOException
	 */
	public static List<WSDLService> createServiceEntries(
			String serviceGroupName, File workspaceFolder, File uniqueFolder,
			File[] wsdlXsdFileArray, boolean create) throws WSDLException,
			MalformedURLException, ParserConfigurationException, SAXException,
			URISyntaxException, TransformerException, IOException {

		List<WSDLService> wsdlServiceList = new ArrayList<WSDLService>();
		String rawXmlData = null;
		WebServices webServices = null;
		WSDLService wsdlService = null;
		Catalog catalog = null;

		createWorkspaceFolder(workspaceFolder);
		File catalogFolder = getCatalogFolder(workspaceFolder);

		CatalogReader parser = null;
		File catalogFile = new File(workspaceFolder,
				CatalogWriter.FILE_NAME_KEY);
		catalog = getCatalog(catalogFile, catalog, workspaceFolder);

		if (catalog != null) {
			webServices = catalog.getWebServices(workspaceFolder
					.getAbsolutePath());
		}

		if (webServices == null) {
			webServices = new WebServices(workspaceFolder);
		}

		// /////////////////////////////////////////////////
		// Store the WSDL and XSD files in a unique folder
		// /////////////////////////////////////////////////

		uniqueFolder = getUniqueFolder(uniqueFolder, workspaceFolder, create);

		List<File> wsdlFileList = new ArrayList<File>();
		List<File> xsdFileList = new ArrayList<File>();

		storeFiles(wsdlXsdFileArray, wsdlFileList, xsdFileList, uniqueFolder,
				create);

		// ///////////////////////////////////////
		// For each Service, create equivalent
		// WSDLService objects and add
		// them to the Catalog for each Service
		// ///////////////////////////////////////

		wsdlServiceList = createWsdlServices(serviceGroupName, uniqueFolder,
				wsdlFileList, xsdFileList, webServices);

		catalog.addWebServicesToCatalog(webServices);
		catalogWrite(catalog, catalogFile);

		return wsdlServiceList;
	}

	public static List<WSDLService> createWsdlServices(String serviceGroupName,
			File wsdlFolder, List<File> wsdlFileList, List<File> xsdFileList,
			WebServices webServices) throws WSDLException {

		List<WSDLService> wsdlServiceList = new ArrayList<WSDLService>();
		// ///////////////////////////////////////
		// For each Service, create equivalent
		// WSDLService objects and add
		// them to the Catalog for each Service
		// ///////////////////////////////////////
		Service service = null;
		Port port = null;
		PortType portType = null;
		Map<QName /*serviceQName*/, Service> servicesMap = null;
		WSDLModel wsdlModel = null;
		for (File aFile : wsdlFileList) {
			wsdlModel = new WSDLModelHelper();
			try {
				wsdlModel.populate(aFile.getAbsolutePath());
			} catch (XmlException e) {
				//e.printStackTrace();
				System.out.println("Exception: " + e.getMessage());
			}
			servicesMap = wsdlModel.getDefinition().getAllServices();
			if (servicesMap.size() > 0) {
				for (QName serviceQName : servicesMap.keySet()) {
					String targetNamespace = serviceQName.getNamespaceURI();
					service = servicesMap.get(serviceQName);
					Map<String /* portName */, Port> portsMap = service
							.getPorts();
					for (String portName : portsMap.keySet()) {
						port = portsMap.get(portName);
						String locationURI = wsdlModel.getSOAPLocationURI(port);
						if (locationURI == null) {
							// Skip since there is no
							// SOAP location URI
							continue;
						}
						portType = port.getBinding().getPortType();
						WSDLService wsdlService = new WSDLService(serviceQName,
								serviceGroupName, targetNamespace, portName,
								locationURI, portType.getQName(), wsdlFolder,
								aFile, wsdlFileList, xsdFileList);
						if (null != webServices
								.addWsdlServiceToList(wsdlService)) {
							wsdlServiceList.add(wsdlService);
						}
					} // end for portName
				} // end for serviceQName
			} // end servicesMap.size() > 0
		} // end for aFile 

		return wsdlServiceList;
	}

	/**
	 * update service group wsdl service entries
	 * @param serviceGroupName
	 * @param workspaceFolder
	 * @param wsdlXsdFileArray
	 * @return
	 * @throws WSDLException
	 * @throws MalformedURLException
	 * @throws ParserConfigurationException
	 * @throws SAXException
	 * @throws URISyntaxException
	 * @throws TransformerException
	 * @throws IOException
	 */
	public static List<WSDLService> updateServiceEntries(
			String serviceGroupName, File workspaceFolder, File wsdlFolder,
			File[] wsdlXsdFileArray) throws WSDLException,
			MalformedURLException, ParserConfigurationException, SAXException,
			URISyntaxException, TransformerException, IOException {

		String rawXmlData = null;
		WebServices webServices = null;

		File catalogFolder = new File(workspaceFolder,
				XmlConstants.CATALOG_CATALOG_KEY);
		File catalogFile = new File(workspaceFolder,
				CatalogWriter.FILE_NAME_KEY);
		Catalog catalog = CatalogReader.getCatalog(catalogFile
				.getAbsolutePath());
		if (catalog != null) {
			webServices = Catalog.getWebServices(catalog, serviceGroupName);
		} else {
			return null;
		}

		List<File> wsdlFileList = new ArrayList<File>();
		List<File> xsdFileList = new ArrayList<File>();
		// /////////////////////////////////////////////////
		//  get the WSDL and XSD files from a wsdl folder
		// /////////////////////////////////////////////////
		for (File aFile : wsdlXsdFileArray) {
			if (aFile.getName().endsWith(AspectsGenericConstants.XSD_SUFFIX)) {
				xsdFileList.add(aFile);
			}
			if (aFile.getName().endsWith(AspectsGenericConstants.WSDL_SUFFIX)) {
				wsdlFileList.add(aFile);
			}
		}

		// ///////////////////////////////////////
		// update services to Catalog
		// ///////////////////////////////////////
		// remove old webservices from catalog
		catalog.removeWebServicesFromCatalog(webServices);
		webServices.removeWSDLServiceList(serviceGroupName);

		// update wsdlservices with new values
		List<WSDLService> wsdlServiceList = null;

		wsdlServiceList = createWsdlServices(serviceGroupName, wsdlFolder,
				wsdlFileList, xsdFileList, webServices);

		// add to catalog
		catalog.addWebServicesToCatalog(webServices);

		// write it out
		catalogWrite(catalog, catalogFile);

		return wsdlServiceList;
	}

	/**
	 * 
	 * @param serviceGroupName
	 * @param workspaceFolder
	 * @param wsdlXsdFileArray
	 * @return
	 * @throws WSDLException
	 * @throws MalformedURLException
	 * @throws ParserConfigurationException
	 * @throws SAXException
	 * @throws URISyntaxException
	 * @throws TransformerException
	 * @throws IOException
	 */
	public static void removeServiceEntries(String serviceGroupName,
			File workspaceFolder, File wsdlFolder, File[] wsdlXsdFileArray)
			throws WSDLException, MalformedURLException,
			ParserConfigurationException, SAXException, URISyntaxException,
			TransformerException, IOException {

		//List<WSDLService> wsdlServiceList = new ArrayList<WSDLService>();
		String rawXmlData = null;
		WebServices webServices = null;
		//WSDLService wsdlService = null;
		File catalogFolder = new File(workspaceFolder,
				XmlConstants.CATALOG_CATALOG_KEY);
		File catalogFile = new File(workspaceFolder,
				CatalogWriter.FILE_NAME_KEY);
		Catalog catalog = CatalogReader.getCatalog(catalogFile
				.getAbsolutePath());
		if (catalog != null) {
			webServices = Catalog.getWebServices(catalog, serviceGroupName);
		}

		// /////////////////////////////////////////////////
		// remove the WSDL and XSD files from a unique folder
		// /////////////////////////////////////////////////
		for (File aFile : wsdlXsdFileArray) {
			if (aFile.getName().endsWith(AspectsGenericConstants.XSD_SUFFIX)
					|| aFile.getName().endsWith(
							AspectsGenericConstants.WSDL_SUFFIX)) {
				File file = new File(wsdlFolder, aFile.getName());
				file.delete();
			}
		}
		// remove wsdl folder if empty
		File[] files = wsdlFolder.listFiles();
		if (files == null || files.length == 0) {
			wsdlFolder.delete();
		}

		// ///////////////////////////////////////
		// remove service from Catalog
		// ///////////////////////////////////////
		catalog.removeWebServicesFromCatalog(webServices);
		webServices.removeWSDLServiceList(serviceGroupName);

		catalogWrite(catalog, catalogFile);
	}

	/**
	 * 
	 * @param serviceGroupName
	 * @param workspaceFolder
	 * @param wsdlxsdZipFile
	 * @return
	 * @throws WSDLException
	 * @throws MalformedURLException
	 * @throws ParserConfigurationException
	 * @throws SAXException
	 * @throws URISyntaxException
	 * @throws TransformerException
	 * @throws IOException
	 */
	public static List<WSDLService> createServiceEntries(
			String serviceGroupName, File workspaceFolder, File wsdlxsdZipFile)
			throws WSDLException, MalformedURLException,
			ParserConfigurationException, SAXException, URISyntaxException,
			TransformerException, IOException {
		if (workspaceFolder.exists() == false) {
			FileUtilities.createFolder(workspaceFolder.getAbsolutePath());
		}
		List<WSDLService> wsdlServiceList = new ArrayList<WSDLService>();
		String rawXmlData = null;
		WebServices webServices = null;
		WSDLService wsdlService = null;
		Catalog catalog = null;

		createWorkspaceFolder(workspaceFolder);
		File catalogFolder = getCatalogFolder(workspaceFolder);

		CatalogReader parser = null;
		File catalogFile = new File(workspaceFolder,
				CatalogWriter.FILE_NAME_KEY);
		catalog = getCatalog(catalogFile, catalog, workspaceFolder);

		if (catalog != null) {
			webServices = catalog.getWebServices(workspaceFolder
					.getAbsolutePath());
		}

		if (webServices == null) {
			webServices = new WebServices(workspaceFolder);
		}

		// ///////////////////////////////////////
		// Store the WSDL file in a unique folder
		// //////////////////////////////////////
		File uniqueFolder = getUniqueFolder(new File("tmpfile"),
				workspaceFolder, true);
		String uniqueFolderString = uniqueFolder.getAbsolutePath();

		List<File> wsdlFileList = new ArrayList<File>();
		List<File> xsdFileList = new ArrayList<File>();
		List<ZipEntry> entries = ZipFileUtility.listFileContents(wsdlxsdZipFile
				.getAbsolutePath());
		for (ZipEntry entry : entries) {
			String name = entry.getName();
			File file = new File(name);
			if (name.endsWith(AspectsGenericConstants.WSDL_SUFFIX) == true) {
				File aFile = ZipFileUtility.readFileFromArchive(wsdlxsdZipFile
						.getAbsolutePath(), name, uniqueFolderString
						+ File.separator + file.getName());
				wsdlFileList.add(aFile);
			}
			if (name.endsWith(AspectsGenericConstants.XSD_SUFFIX) == true) {
				File aFile = ZipFileUtility.readFileFromArchive(wsdlxsdZipFile
						.getAbsolutePath(), name, uniqueFolderString
						+ File.separator + file.getName());
				xsdFileList.add(aFile);
			}
		}

		// ///////////////////////////////////////
		// For each Service, create equivalent
		// WSDLService objects and add
		// them to the Catalog for each Service
		// ///////////////////////////////////////

		wsdlServiceList = createWsdlServices(serviceGroupName, uniqueFolder,
				wsdlFileList, xsdFileList, webServices);

		catalog.addWebServicesToCatalog(webServices);
		catalogWrite(catalog, catalogFile);

		return wsdlServiceList;
	}

	private static void catalogWrite(Catalog catalog, File catalogFile)
			throws ParserConfigurationException, IOException,
			TransformerException {
		CatalogWriter serializer = new CatalogWriter();
		String rawXmlData = serializer.serialize(catalog);
		serializer.setContents(catalogFile, rawXmlData);
	}

	/**
	 * 
	 * @param catalog
	 * @param serviceQNameString
	 * @return
	 */
	public static List<WSDLService> findWSDLServiceList(Catalog catalog,
			String serviceQNameString) {
		List<WSDLService> wsdlServiceList = new ArrayList<WSDLService>();

		List<WebServices> servicesList = catalog.getWebServicesList();
		for (WebServices webServices : servicesList) {
			wsdlServiceList = webServices
					.getWsdlServiceList(serviceQNameString);
			if ((wsdlServiceList != null) && (wsdlServiceList.size() > 0)) {
				break;
			}
		}
		return wsdlServiceList;
	}

	/**
	 * 
	 * @param catalog
	 * @return
	 */
	public static String constructInputFromCatalog(Catalog catalog) {
		String inputString = "";

		for (WebServices webServices : catalog.getWebServicesList()) {
			for (WSDLService service : webServices.getWsdlServiceList()) {
				String result = constructInputForService(service);
				if (result != null) {
					inputString += result;
				}
			}
		}

		return inputString;
	}

	/**
	 * 
	 * @param catalog
	 * @param serviceQNameString
	 * @return
	 */
	public static String constructInputForService(Catalog catalog,
			String serviceQNameString) {
		String inputString = "";
		List<WSDLService> wsdlServiceList = findWSDLServiceList(catalog,
				serviceQNameString);
		for (WSDLService service : wsdlServiceList) {
			String result = constructInputForService(service);
			if (result != null) {
				inputString += result;
			}
		}
		return inputString;
	}

	/**
	 * 
	 * @param catalog
	 * @param serviceQNameString
	 * @return
	 */
	public static String constructInputForService(WSDLService service) {
		String inputString = "";
		if ((service.getServiceWSDLFile() != null)
				&& (service.getFolder() != null)
				&& (service.getLocationURI() != null)
				&& (service.getName() != null)
				&& (service.getPortName() != null)
				&& (service.getPortType() != null)
				&& (service.getServiceGroupName() != null)
				&& (service.getTargetNamespace() != null)) {
			inputString = service.getTargetNamespace() + ";"
					+ service.getName() + ";" + service.getPortName() + ";"
					+ service.getPortType() + ";" + service.getLocationURI()
					+ ";";
			String xsdFileList = "";
			for (File file : service.getXsdFiles()) {
				xsdFileList += file.getAbsolutePath() + " ";
			}
			inputString += xsdFileList;
			inputString += "|";
		}
		return inputString;
	}

	/**
	 * 
	 * @param catalog
	 * @param serviceGroupName
	 * @return
	 */
	public static List<WSDLService> getWSDLServiceList(Catalog catalog,
			String serviceGroupName) {
		List<WSDLService> result = null;
		List<WebServices> servicesList = catalog.getWebServicesList();
		for (WebServices services : servicesList) {
			result = services.getWSDLServiceList(serviceGroupName);
			if (result != null) {
				break;
			}
		}
		return result;
	}

	/**
	 * 
	 * @param catalog
	 * @param serviceGroupName
	 * @return WebServices
	 */
	public static WebServices getWebServices(Catalog catalog,
			String serviceGroupName) {
		WebServices webServices = null;
		List<WebServices> servicesList = catalog.getWebServicesList();
		for (WebServices services : servicesList) {
			List<WSDLService> wsdlServiceList = services
					.getWSDLServiceList(serviceGroupName);
			if (wsdlServiceList != null) {
				webServices = services;
				break;
			}
		}
		return webServices;
	}

	/** get a list of ServiceGroup names
	 * @param catalog
	 * @return  List<String> of ServiceGroup names
	 */
	public static List<String> getServiceGroupList(Catalog catalog) {
		List serviceGroupList = new ArrayList<String>();
		List<WebServices> servicesList = catalog.getWebServicesList();
		for (WebServices services : servicesList) {
			List<WSDLService> wsdlServiceList = services.getWsdlServiceList();
			for (WSDLService wsdlService : wsdlServiceList) {
				String serviceGroupName = wsdlService.serviceGroupName;
				if (serviceGroupList.contains(serviceGroupName) == false) {
					serviceGroupList.add(serviceGroupName);
				}
			}
		}
		return serviceGroupList;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		String wsdlURI = "http://webservices.amazon.com/AWSECommerceService/AWSECommerceService.wsdl";
		String workspaceLocation = System.getProperty("JBICOMPS_HOME") + "/camplugins/aspects/build/web/workspace";
		File workspaceFolder = new File(workspaceLocation);
		List<WSDLService> serviceList = null;

		// ////////////////
		// Test 1
		// ////////////////

		try {
			serviceList = Catalog.createServiceEntries("serviceGroupName1",
					workspaceFolder, wsdlURI);
			for (WSDLService wsdlService : serviceList) {
				System.out.println("    Location URI: "
						+ wsdlService.getLocationURI());
				System.out.println("    Port Name: "
						+ wsdlService.getPortName());
				System.out.println("    TargetNamespace: "
						+ wsdlService.getTargetNamespace());
				System.out.println("    FolderName: "
						+ wsdlService.getFolder().getAbsolutePath());
				System.out.println("    Name: " + wsdlService.getName());
				System.out.println("    System Group Name: "
						+ wsdlService.getServiceGroupName());
				System.out
						.println("    PortType: " + wsdlService.getPortType());
				System.out.println("    Service WSDL File: "
						+ wsdlService.getServiceWSDLFile().getAbsolutePath());

				List<File> wsdlFileList = wsdlService.getWsdlFiles();
				for (File file : wsdlFileList) {
					System.out.println("      WSDL File: "
							+ file.getAbsolutePath());
				}
				List<File> xsdFileList = wsdlService.getXsdFiles();
				for (File file : xsdFileList) {
					System.out.println("      XSD File: "
							+ file.getAbsolutePath());
				}
			}
		} catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (WSDLException e) {
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
		} catch (TransformerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		// ////////////////
		// Test 2
		// ////////////////

		String folderName = "C:/test/aspects/demo/AspectApplication/src";
		File folder = new File(folderName);
		if (folder.isDirectory() == true) {
			File[] fileList = folder.listFiles();
			try {
				serviceList = Catalog.createServiceEntries("serviceGroupName2",
						workspaceFolder, folder, fileList);
				for (WSDLService wsdlService : serviceList) {
					System.out.println("    Location URI: "
							+ wsdlService.getLocationURI());
					System.out.println("    Port Name: "
							+ wsdlService.getPortName());
					System.out.println("    TargetNamespace: "
							+ wsdlService.getTargetNamespace());
					System.out.println("    FolderName: "
							+ wsdlService.getFolder().getAbsolutePath());
					System.out.println("    Name: " + wsdlService.getName());
					System.out.println("    System Group Name: "
							+ wsdlService.getServiceGroupName());
					System.out.println("    PortType: "
							+ wsdlService.getPortType());
					System.out.println("    Service WSDL File: "
							+ wsdlService.getServiceWSDLFile()
									.getAbsolutePath());

					List<File> wsdlFileList = wsdlService.getWsdlFiles();
					for (File file : wsdlFileList) {
						System.out.println("      WSDL File: "
								+ file.getAbsolutePath());
					}
					List<File> xsdFileList = wsdlService.getXsdFiles();
					for (File file : xsdFileList) {
						System.out.println("      XSD File: "
								+ file.getAbsolutePath());
					}
				}
			} catch (MalformedURLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (WSDLException e) {
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
			} catch (TransformerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		// ////////////////
		// Test 3
		// ////////////////

		folderName = "C:/test/aspects/demo/";
		folder = new File(folderName);
		if (folder.isDirectory() == true) {
			File wsdlxsdZipFile = new File(folder, "src.zip");
			try {
				serviceList = Catalog.createServiceEntries("serviceGroupName3",
						workspaceFolder, wsdlxsdZipFile);
				for (WSDLService wsdlService : serviceList) {
					System.out.println("    Location URI: "
							+ wsdlService.getLocationURI());
					System.out.println("    Port Name: "
							+ wsdlService.getPortName());
					System.out.println("    TargetNamespace: "
							+ wsdlService.getTargetNamespace());
					System.out.println("    FolderName: "
							+ wsdlService.getFolder().getAbsolutePath());
					System.out.println("    Name: " + wsdlService.getName());
					System.out.println("    System Group Name: "
							+ wsdlService.getServiceGroupName());
					System.out.println("    PortType: "
							+ wsdlService.getPortType());
					System.out.println("    Service WSDL File: "
							+ wsdlService.getServiceWSDLFile()
									.getAbsolutePath());

					List<File> wsdlFileList = wsdlService.getWsdlFiles();
					for (File file : wsdlFileList) {
						System.out.println("      WSDL File: "
								+ file.getAbsolutePath());
					}
					List<File> xsdFileList = wsdlService.getXsdFiles();
					for (File file : xsdFileList) {
						System.out.println("      XSD File: "
								+ file.getAbsolutePath());
					}
				}
			} catch (MalformedURLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (WSDLException e) {
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
			} catch (TransformerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		// ////////////////
		// Test 4
		// ////////////////
		String uri = workspaceLocation + "/catalog.xml";
		CatalogReader parser = null;
		try {
			parser = CatalogReader.parseFromFile(uri);
			Catalog catalog = parser.getCatalog();
			String input = Catalog.constructInputFromCatalog(catalog);
			System.out.println(input);
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

	}

}
