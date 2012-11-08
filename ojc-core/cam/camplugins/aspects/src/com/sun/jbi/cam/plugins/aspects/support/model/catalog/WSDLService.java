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
 * @(#)WSDLService.java 
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
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import com.sun.jbi.cam.common.EqualsUtil;
import com.sun.jbi.cam.common.HashCodeUtil;


/**
 * @author graj
 *<WSDLService 
 *name="{tns1}service1" 
 *targetNamespace="tns1" port="portName" locationURI="http://localhost:8080/service1" 
 *portType="{tns1}portType1" 
 *folderName="01000000-4D391F5C110100-8199A797-01" wsdlFiles="wsdl1 wsdl2 wsdl3 wsdl4"
 *serviceWSDLFile="01000000-4D391F5C110100-8199A797-01/wsdl1" 
 *xsdFiles="xsd1 xsd2 xsd3" />
 */
public class WSDLService implements Serializable {
	private static final long serialVersionUID = 1L;

	QName name;
	String serviceGroupName;
	String targetNamespace;
	String portName;
	String locationURI;
	QName portType;
	File folder;
	File serviceWSDLFile;
	List<File> wsdlFiles = new ArrayList<File>();
	List<File> xsdFiles = new ArrayList<File>();

	/**
	 * 
	 */
	public WSDLService() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param name
	 * @param serviceGroupName
	 * @param targetNamespace
	 * @param portName
	 * @param locationURI
	 * @param portType
	 * @param folder
	 * @param serviceWSDLFile
	 * @param wsdlFiles
	 * @param xsdFiles
	 */
	public WSDLService(QName name, String serviceGroupName, String targetNamespace, String portName, String locationURI, QName portType, File folder, File serviceWSDLFile, List<File> wsdlFiles, List<File> xsdFiles) {
		this.name = name;
		this.serviceGroupName = serviceGroupName;
		this.targetNamespace = targetNamespace;
		this.portName = portName;
		this.locationURI = locationURI;
		this.portType = portType;
		this.folder = folder;
		this.serviceWSDLFile = serviceWSDLFile;
		this.wsdlFiles = wsdlFiles;
		this.xsdFiles = xsdFiles;
	}
	
	/**
	 * @param name
	 * @param serviceGroupName
	 * @param targetNamespace
	 * @param portName
	 * @param locationURI
	 * @param portType
	 * @param folder
	 * @param serviceWSDLFile
	 * @param wsdlFiles
	 * @param xsdFiles
	 */
	public WSDLService(String nameString, 
			String serviceGroupName, 
			String targetNamespace, 
			String portName, 
			String locationURI, 
			String portTypeString,
			String baseLocation,
			String folderNameString, 
			String serviceWSDLFileString, 
			String spaceSeparatedWsdlFiles, 
			String spaceSeparatedXsdFiles) {
        this.name = QName.valueOf(nameString);
        this.serviceGroupName = serviceGroupName;
        this.targetNamespace = targetNamespace;
        this.portName = portName;
        this.locationURI = locationURI;
        this.portType = QName.valueOf(portTypeString);
        this.folder = new File(new File(baseLocation), folderNameString);
        this.serviceWSDLFile = new File(new File(baseLocation), serviceWSDLFileString);
        if((spaceSeparatedWsdlFiles != null)
                && (spaceSeparatedWsdlFiles.trim().length() > 0)) {
            String[] wsdlFileArray = spaceSeparatedWsdlFiles.split(" ");
            for(String wsdlFileString : wsdlFileArray) {
                this.wsdlFiles.add(new File(wsdlFileString));
            }
        }
        if((spaceSeparatedXsdFiles != null)
                && (spaceSeparatedXsdFiles.trim().length() > 0)) {
            String[] xsdFileArray = spaceSeparatedXsdFiles.split(" ");
            for(String xsdFileString : xsdFileArray) {
                this.xsdFiles.add(new File(xsdFileString));
            }
        }
	}	




	/**
	 * @param name
	 * @param targetNamespace
	 * @param portName
	 * @param locationURI
	 * @param portType
	 * @param folder
	 * @param wsdlFiles
	 * @param xsdFiles
	 */
	public WSDLService(QName name, 
			String targetNamespace, 
			String portName, 
			String locationURI, 
			QName portType, 
			File folderName, 
			List<File> wsdlFiles, 
			List<File> xsdFiles) {
		this.name = name;
		this.targetNamespace = targetNamespace;
		this.portName = portName;
		this.locationURI = locationURI;
		this.portType = portType;
		this.folder = folderName;
		this.wsdlFiles = wsdlFiles;
		this.xsdFiles = xsdFiles;
	}



	/**
	 * @return the folder
	 */
	public File getFolder() {
		return folder;
	}



	/**
	 * @param folder the folder to set
	 */
	public void setFolder(File folderName) {
		this.folder = folderName;
	}



	/**
	 * @return the locationURI
	 */
	public String getLocationURI() {
		return locationURI;
	}



	/**
	 * @param locationURI the locationURI to set
	 */
	public void setLocationURI(String locationURI) {
		this.locationURI = locationURI;
	}



	/**
	 * @return the name
	 */
	public QName getName() {
		return name;
	}



	/**
	 * @param name the name to set
	 */
	public void setName(QName name) {
		this.name = name;
	}



	/**
	 * @return the portName
	 */
	public String getPortName() {
		return portName;
	}



	/**
	 * @param portName the portName to set
	 */
	public void setPortName(String portName) {
		this.portName = portName;
	}



	/**
	 * @return the portType
	 */
	public QName getPortType() {
		return portType;
	}



	/**
	 * @param portType the portType to set
	 */
	public void setPortType(QName portType) {
		this.portType = portType;
	}



	/**
	 * @return the targetNamespace
	 */
	public String getTargetNamespace() {
		return targetNamespace;
	}



	/**
	 * @param targetNamespace the targetNamespace to set
	 */
	public void setTargetNamespace(String targetNamespace) {
		this.targetNamespace = targetNamespace;
	}



	/**
	 * @return the wsdlFiles
	 */
	public List<File> getWsdlFiles() {
		return wsdlFiles;
	}



	/**
	 * @param wsdlFiles the wsdlFiles to set
	 */
	public void setWsdlFiles(List<File> wsdlFiles) {
		this.wsdlFiles = wsdlFiles;
	}

	/**
	 * @param spaceSeparatedWsdlFiles the spaceSeparatedWsdlFiles to set
	 */
	public void setWsdlFiles(String spaceSeparatedWsdlFiles) {
		if((spaceSeparatedWsdlFiles != null)
				&& (spaceSeparatedWsdlFiles.trim().length() > 0)) {
			String[] wsdlFileArray = spaceSeparatedWsdlFiles.split(" ");
			for(String wsdlFileString : wsdlFileArray) {
				this.wsdlFiles.add(new File(wsdlFileString));
			}
		}
	}

	/**
	 * 
	 * @param baseLocation
	 * @param wsdlFileString
	 */
	public void addWsdlFile(String baseLocation, String wsdlFileString) {
		if((wsdlFileString != null)
				&& (wsdlFileString.trim().length() > 0)) {
			this.wsdlFiles.add(new File(new File(baseLocation), wsdlFileString));
		}
	}

	/**
	 * @return the xsdFiles
	 */
	public List<File> getXsdFiles() {
		return xsdFiles;
	}



	/**
	 * @param xsdFiles the xsdFiles to set
	 */
	public void setXsdFiles(List<File> xsdFiles) {
		this.xsdFiles = xsdFiles;
	}

	/**
	 * @param spaceSeparatedWsdlFiles the spaceSeparatedWsdlFiles to set
	 */
	public void setXsdFiles(String spaceSeparatedXsdFiles) {
		if((spaceSeparatedXsdFiles != null)
				&& (spaceSeparatedXsdFiles.trim().length() > 0)) {
			String[] xsdFileArray = spaceSeparatedXsdFiles.split(" ");
			for(String xsdFileString : xsdFileArray) {
				this.xsdFiles.add(new File(xsdFileString));
			}
		}
	}
	
	/**
	 * 
	 * @param baseLocation
	 * @param xsdFileString
	 */
	public void addXsdFile(String baseLocation, String xsdFileString) {
		if((xsdFileString != null)
				&& (xsdFileString.trim().length() > 0)) {
			this.xsdFiles.add(new File(new File(baseLocation), xsdFileString));
		}
	}
	
	/**
	 * @return the serviceWSDLFile
	 */
	public File getServiceWSDLFile() {
		return serviceWSDLFile;
	}

	/**
	 * @param serviceWSDLFile the serviceWSDLFile to set
	 */
	public void setServiceWSDLFile(File serviceWSDLFile) {
		this.serviceWSDLFile = serviceWSDLFile;
	}

	
	/**
	 * @return the serviceGroupName
	 */
	public String getServiceGroupName() {
		return serviceGroupName;
	}


	/**
	 * @param serviceGroupName the serviceGroupName to set
	 */
	public void setServiceGroupName(String serviceGroupName) {
		this.serviceGroupName = serviceGroupName;
	}


	public boolean equals(Object aThat) {
		// check for self-comparison
		if (this == aThat) {
			return true;
		}

		// use instanceof instead of getClass here for two reasons
		// 1. if need be, it can match any supertype, and not just one class;
		// 2. it renders an explict check for "that == null" redundant, since
		// it does the check for null already - "null instanceof [type]" always
		// returns false. (See Effective Java by Joshua Bloch.)
		if (!(aThat instanceof WSDLService)) {
			return false;
		}
		// Alternative to the above line :
		// if ( aThat == null || aThat.getClass() != this.getClass() ) return
		// false;

		// cast to native object is now safe
		WSDLService that = (WSDLService) aThat;

		// now a proper field-by-field evaluation can be made
		return EqualsUtil.areEqual(this.name, that.name)
				&& EqualsUtil.areEqual(this.targetNamespace, that.targetNamespace)
				&& EqualsUtil.areEqual(this.portName, that.portName)
		&& EqualsUtil.areEqual(this.locationURI, that.locationURI)
		&& EqualsUtil.areEqual(this.portType, that.portType)
		&& EqualsUtil.areEqual(this.folder, that.folder)
		&& EqualsUtil.areEqual(this.wsdlFiles, that.wsdlFiles)
		&& EqualsUtil.areEqual(this.xsdFiles, that.xsdFiles);
		
	}

	public int hashCode() {
		int result = HashCodeUtil.SEED;
		result = HashCodeUtil.hash(result, this.name);
		result = HashCodeUtil.hash(result, this.targetNamespace);
		result = HashCodeUtil.hash(result, this.portName);
		result = HashCodeUtil.hash(result, this.locationURI);
		result = HashCodeUtil.hash(result, this.portType);
		result = HashCodeUtil.hash(result, this.folder);
		result = HashCodeUtil.hash(result, this.wsdlFiles);
		result = HashCodeUtil.hash(result, this.xsdFiles);
		return result;
	}


	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
