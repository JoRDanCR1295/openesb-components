 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.xml.namespace.QName;

/**
 * Used to store some WSDL information.
 */
public class WsdlInformation {

  // has a ...

  /** The list of the interfaces defined in the WSDL. */
  private List<QName> portTypeList = new ArrayList<QName>();

  /**
   * A map where the key is the name of the Service and the value is the
   * associated PortType.
   */
  Map<QName, QName> serviceAndPortType = new HashMap<QName, QName>();

  /**
   * A map where the key is the QName of an interface and the value is a list
   * of name of the asynchronous operations associated.
   */
  private Map<QName, List<QName>> asynchOperationMap
    = new HashMap<QName, List<QName>>();

  /**
   * A map where the key is the QName of an interface and the value is a list
   * of name of the synchronous operations associated.
   */
  private Map<QName, List<QName>> synchOperationMap
    = new HashMap<QName, List<QName>>();

  /**
   * Constructor.
   */
  public WsdlInformation() {
    // NOP
  }

  // getter and setter

  public List<QName> getPortTypeList() {
      return portTypeList;
  }

  public void setPortTypeList(List<QName> portTypeList) {
      this.portTypeList = portTypeList;
  }

  public Map<QName, List<QName>> getAsynchOperationMap() {
      return asynchOperationMap;
  }

  public void setAsynchOperationMap(
    Map<QName, List<QName>> asynchOperationMap) {

    this.asynchOperationMap = asynchOperationMap;
  }

  public Map<QName, List<QName>> getSynchOperationMap() {
      return synchOperationMap;
  }

  public void setSynchOperationMap(Map<QName, List<QName>> synchOperationMap) {
      this.synchOperationMap = synchOperationMap;
  }

  public Map<QName, QName> getServiceAndPortType() {
      return serviceAndPortType;
  }

  public void setServiceAndPortType(Map<QName, QName> serviceAndPortType) {
      this.serviceAndPortType = serviceAndPortType;
  }

}
