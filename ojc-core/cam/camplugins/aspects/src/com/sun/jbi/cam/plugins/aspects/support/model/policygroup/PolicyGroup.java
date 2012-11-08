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
 * @(#)PolicyGroup.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.plugins.aspects.support.model.policygroup;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

import com.sun.jbi.cam.plugins.aspects.common.XmlConstants;
import com.sun.jbi.cam.plugins.aspects.support.model.Advice;
import com.sun.jbi.cam.plugins.aspects.support.model.AdviceConfiguration;
import com.sun.jbi.cam.plugins.aspects.support.model.FacadeConfiguration;
import com.sun.jbi.cam.plugins.aspects.support.model.ProviderConfiguration;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.Catalog;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.xml.CatalogReader;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.xml.CatalogWriter;

/*
 * policyGroupName, Policy Group 1
 * export_operation, <?xml version="1.0" encoding="UTF-8"?><aspectmap></aspectmap>
 * export_asp8_facade_1, <?xml version="1.0" encoding="UTF-8"?><facade><config><property name="serviceName" value="serviceName-sn" /><property name="portName" value="portName-pn" /><property name="soap:address" value="http://www.soap.com" /></config></facade>
 * export_asp5_mta_2, <?xml version="1.0" encoding="UTF-8"?><messageTracking><config><property name="mode" value="file" /></config></messageTracking>
 */
/**
 * @author graj
 *
 */
public class PolicyGroup implements Serializable {
    private static final long serialVersionUID = 1L;
	File baseLocation;
	String name = "";
    String policyGroupName = "";
    String operationXml = "";
    String inputString = "";
    String facadeString = "";
    String aspectString = "";
    
	/**
	 * 
	 */
	public PolicyGroup() {
		// TODO Auto-generated constructor stub
	}
	
	/**
	 * @param baseLocation
	 * @param name
	 * @param policyGroupName
	 * @param operationXml
	 * @param inputString
	 * @param facadeString
	 * @param aspectString
	 */
	public PolicyGroup(File baseLocation, String name, String policyGroupName, String operationXml, String inputString, String facadeString, String aspectString) {
		this.baseLocation = baseLocation;
		this.name = name;
		this.policyGroupName = policyGroupName;
		this.operationXml = operationXml;
		this.inputString = inputString;
		this.facadeString = facadeString;
		this.aspectString = aspectString;
	}




	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}




	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}




	/**
	 * @return the aspectString
	 */
	public String getAspectString() {
		return aspectString;
	}
	
	/**
	 * 
	 * @param key
	 * @param configuration
	 */
	public void addToAdviceString(String key, AdviceConfiguration configuration) {
		//"export_asp5_mta_2;file;;;|export_asp4_la_3;WARNING;WEEKLY;/tmp/loggingse.log;|export_asp0_ca_4;GenericCache;95;;|"
		String keyValueString = "";
                if((key != null) && (configuration != null)) {
                    keyValueString = key+";"+configuration.retrieveValuesAsString()+"|";
                    this.aspectString += keyValueString; 
                }
		
	}




	/**
	 * @param aspectString the aspectString to set
	 */
	public void setAspectString(String aspectString) {
		this.aspectString = aspectString;
	}




	/**
	 * @return the baseLocation
	 */
	public File getBaseLocation() {
		return baseLocation;
	}




	/**
	 * @param baseLocation the baseLocation to set
	 */
	public void setBaseLocation(File baseLocation) {
		this.baseLocation = baseLocation;
	}




	/**
	 * @return the facadeString
	 */
	public String getFacadeString() {
		return facadeString;
	}

	/**
	 * 
	 * @param configuration
	 */
	public void addToFacadeString(FacadeConfiguration configuration) {
		this.facadeString = configuration.retrieveValuesAsString();
	}


	/**
	 * @param facadeString the facadeString to set
	 */
	public void setFacadeString(String facadeString) {
		this.facadeString = facadeString;
	}




	/**
	 * @return the inputString
	 */
	public String getInputString() {
		return inputString;
	}
	
	/**
	 * 
	 * @param workspaceFolder
	 * @param configuration
	 */
	public void addToInputString(File workspaceFolder, ProviderConfiguration configuration) {
		Catalog catalog = null;
		String uri = "";
		CatalogReader parser = null;
		
		try {
			uri = workspaceFolder.getAbsolutePath()+ File.separator + CatalogWriter.FILE_NAME_KEY;
			parser = CatalogReader.parseFromFile(uri);
			catalog = parser.getCatalog();		
			this.inputString = Catalog.constructInputForService(catalog, configuration.getServiceQName().toString());
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




	/**
	 * @param inputString the inputString to set
	 */
	public void setInputString(String inputString) {
		this.inputString = inputString;
	}




	/**
	 * @return the operationXml
	 */
	public String getOperationXml() {
		return operationXml;
	}




	/**
	 * @param operationXml the operationXml to set
	 */
	public void setOperationXml(String operationXml) {
		this.operationXml = operationXml;
	}




	/**
	 * @return the policyGroupName
	 */
	public String getPolicyGroupName() {
		return policyGroupName;
	}




	/**
	 * @param policyGroupName the policyGroupName to set
	 */
	public void setPolicyGroupName(String policyGroupName) {
		this.policyGroupName = policyGroupName;
	}




	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
