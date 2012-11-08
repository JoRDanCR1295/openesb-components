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
 * @(#)WebServices.java 
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

/**
 * @author graj
 *
 */
public class WebServices implements Serializable {
	private static final long serialVersionUID = 1L;
	
	File baseLocation;
	//Map<String /*serviceQname*/, WSDLService> nameToServiceMap = new HashMap<String, WSDLService>();
	List<WSDLService> wsdlServiceList = new ArrayList<WSDLService>();
	Map<String /*serviceQName*/, File /*uniqueFolder*/> nameToUniqueFolderMap = new HashMap<String /*serviceQName*/, File /*uniqueFolder*/>();
	Map<String /*serviceGroupName*/, List<WSDLService>> groupToServiceMap = new HashMap<String /*serviceGroupName*/, List<WSDLService>>();
	

	/**
	 * 
	 */
	public WebServices() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param baseLocation
	 */
	public WebServices(File baseLocation) {
		this.baseLocation = baseLocation;
	}

	/**
	 * @param baseLocationString
	 */
	public WebServices(String baseLocationString) {
		this.baseLocation = new File(baseLocationString);
	}
	
	/**
	 * @param baseLocation
	 * @param wsdlServiceList
	 */
	public WebServices(File baseLocation, List<WSDLService> serviceList) {
		this.baseLocation = baseLocation;
		this.wsdlServiceList = serviceList;
		for(WSDLService wsdlService : wsdlServiceList) {
			this.nameToUniqueFolderMap.put(wsdlService.getName().toString(), wsdlService.getFolder());
		}
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
	 * @param baseLocation the baseLocation to set
	 */
	public void setBaseLocation(String baseLocationString) {
		this.baseLocation = new File(baseLocationString);
	}

	/**
	 * @return the wsdlServiceList
	 */
	public List<WSDLService> getWsdlServiceList() {
//		List<WSDLService> wsdlServiceList = new ArrayList<WSDLService>();
//		for(WSDLService wsdlService : this.nameToServiceMap.values()) {
//			wsdlServiceList.add(wsdlService);
//		}
		return this.wsdlServiceList;
	}
	
	/**
	 * 
	 * @param service
	 * @return
	 */
	public boolean addWSDLServiceToServiceGroup(WSDLService service) {
		boolean result = false;
                if((service.getFolder() == null)
                || (service.getLocationURI() == null)
                || (service.getName() == null)
                || (service.getPortName() == null)
                || (service.getPortType() == null)
                || (service.getServiceGroupName() == null)
                || (service.getServiceWSDLFile() == null)
                || (service.getTargetNamespace() == null)) {
                    return false;
                }
		String serviceGroupName = service.getServiceGroupName();
		List<WSDLService> serviceList = this.groupToServiceMap.get(serviceGroupName);
		if(serviceList == null) {
			serviceList = new ArrayList<WSDLService>();
		}
		result = serviceList.add(service);
		this.groupToServiceMap.put(serviceGroupName, serviceList);
		return result;
	}

	/**
	 * 
	 * @param service
	 * @return
	 */
	public boolean removeWSDLServiceFromServiceGroup(WSDLService service) {
		boolean result = false;
		String serviceGroupName = service.getServiceGroupName();
		List<WSDLService> servicesList = this.groupToServiceMap.get(serviceGroupName);
		if(servicesList != null) {
			result = servicesList.remove(service);
		}
		return result;
	}
	

	/**
	 * @param wsdlServiceList the wsdlServiceList to set
	 */
	public void setWsdlServiceList(List<WSDLService> serviceList) {
		for(WSDLService wsdlService : wsdlServiceList) {
			this.nameToUniqueFolderMap.put(wsdlService.getName().toString(), wsdlService.getFolder());
		}
		for(WSDLService service : serviceList) {
			this.addWSDLServiceToServiceGroup(service);
		}
		this.wsdlServiceList = serviceList;
	}

	/**
	 * @param wsdlServiceList the wsdlServiceList to set
	 */
	public WSDLService addWsdlServiceToList(WSDLService wsdlService) {
		WSDLService result = null;
		WSDLService gotIt = null;
                
                if((wsdlService.getFolder() == null)
                || (wsdlService.getLocationURI() == null)
                || (wsdlService.getName() == null)
                || (wsdlService.getPortName() == null)
                || (wsdlService.getPortType() == null)
                || (wsdlService.getServiceGroupName() == null)
                || (wsdlService.getServiceWSDLFile() == null)
                || (wsdlService.getTargetNamespace() == null)) {
                    return null;
                }                
		
		File uniqueFolder = this.nameToUniqueFolderMap.get(wsdlService.getName().toString());
		if(uniqueFolder == null) {
			this.nameToUniqueFolderMap.put(wsdlService.getName().toString(), wsdlService.getFolder());
			this.addWSDLServiceToServiceGroup(wsdlService);
			this.wsdlServiceList.add(wsdlService);
			result = wsdlService;
		} else {
			List<WSDLService> serviceList = this.getWsdlServiceList(wsdlService.getName().toString());
			for(WSDLService service : serviceList) {
				if(service.getName().equals(wsdlService.getName()) 
						&& service.getPortName().equals(wsdlService.getPortName())) {
					gotIt = service;
					break;
				}
			}
			if(gotIt != null) {
				serviceList.remove(gotIt);
				this.nameToUniqueFolderMap.put(wsdlService.getName().toString(), wsdlService.getFolder());
				this.wsdlServiceList.add(wsdlService);
				this.addWSDLServiceToServiceGroup(wsdlService);
				uniqueFolder = gotIt.getFolder();
//				if(uniqueFolder.getName().equals(wsdlService.getFolder().getName())) {
//					if(uniqueFolder.exists() == true) {
//						uniqueFolder.delete();
//					}
//				}
			}
		}
		return result;
	}
	
	/**
	 * @param webServices the webServices to remove
	 */
	public void removeWsdlServiceFromList(WSDLService wsdlService) {
		this.nameToUniqueFolderMap.remove(wsdlService.getName().toString());
		this.wsdlServiceList.remove(wsdlService);
		this.removeWSDLServiceFromServiceGroup(wsdlService);
	}
	
	/**
	 * 
	 * @param serviceQNameString
	 * @return
	 */
	public List<WSDLService> getWsdlServiceList(String serviceQNameString) {
//		return this.nameToServiceMap.get(serviceQNameString);
		List<WSDLService> result = new ArrayList<WSDLService>();
		for(WSDLService wsdlService : this.wsdlServiceList) {
			if(wsdlService.getName().equals(QName.valueOf(serviceQNameString))) {
				result.add(wsdlService);
			}
		}
		return result;
	}
	
	/**
	 * 
	 * @param serviceQName
	 * @return
	 */
	public File getUniqueFolder(QName serviceQName) {
		return this.nameToUniqueFolderMap.get(serviceQName);
	}
	
	/**
	 * 
	 * @param serviceGroupName
	 * @return
	 */
	public List<WSDLService> getWSDLServiceList(String serviceGroupName) {
		return this.groupToServiceMap.get(serviceGroupName);
	}

        public void removeWSDLServiceList(String serviceGroupName) {
            List<WSDLService> wsdlServiceList = getWSDLServiceList(serviceGroupName);
            if ( wsdlServiceList!=null ) {
                // make a copy of wsdlServiceList
                List<WSDLService> list = new ArrayList<WSDLService>(); 
                list.addAll(wsdlServiceList);
                for ( WSDLService wsdlService : list ) {
                    removeWsdlServiceFromList(wsdlService);
                }
            }
        }
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
