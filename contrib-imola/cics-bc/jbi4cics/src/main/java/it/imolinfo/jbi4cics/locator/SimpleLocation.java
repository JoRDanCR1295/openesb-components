/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
/**
 * 
 */
package it.imolinfo.jbi4cics.locator;

import it.imolinfo.jbi4cics.exception.LocationException;
import it.imolinfo.jbi4cics.jbi.Messages;


/**
 * @author raffaele
 *
 */
public class SimpleLocation implements ServiceLocation {
  
  public final static String DUMMY_TYPE="DUMMY";
  public final static String CICS_TYPE="CICS";
  public final static String IMS_TYPE="IMS";
  public final static String JDBC_TYPE="JDBC";
  /*public final static String JMS_TYPE="JMS";
  public final static String MQ_TYPE="MQ";*/  
  
  /**
   * The responsible to translate localized messages.
   */
  private static final Messages MESSAGES
          = Messages.getMessages(SimpleLocation.class);
  
  String locationName;
  int connectionType;
  
  
  /**
   * 
   */
  public SimpleLocation() {
    super();
    // TODO Auto-generated constructor stub
  }

  /* (non-Javadoc)
   * @see it.imolinfo.jbi4cics.locator.ServiceLocation#getJNDIName()
   */
  public String getLocationName() {
    // TODO Auto-generated method stub
    return locationName;
  }
  
  public void setLocationName(String locationName) {
    // TODO Auto-generated method stub
    this.locationName=locationName;
  }  

  /* (non-Javadoc)
   * @see it.imolinfo.jbi4cics.locator.ServiceLocation#getConnectionType()
   */
  public int getConnectionType() {
    // TODO Auto-generated method stub
    return connectionType;
  }
  
  public void setConnectionType(int connectionType) {
    this.connectionType=connectionType;
  }

  /**
   * @return The connectionTypeName
   * @throws LocationException The locsation Exception
   */
  public String getConnectionTypeName() throws LocationException {
    switch (getConnectionType()) {
      case ServiceLocation.DUMMY : return DUMMY_TYPE;
      case ServiceLocation.CICS  : return CICS_TYPE;
      case ServiceLocation.IMS   : return IMS_TYPE;
      case ServiceLocation.JDBC  : return JDBC_TYPE;
      /*case ServiceLocation.JMS   : return JMS_TYPE;
      case ServiceLocation.MQ    : return MQ_TYPE;*/
      default : throw new LocationException(MESSAGES.getString(
              "CIC001500_Unknown_type", getConnectionType()));
    }
  }

  /**
   * @param connectionTypeName The connectionTypeName to set
   * @throws LocationException The location exception
   */
  public void setConnectionTypeName(String connectionTypeName) throws LocationException {
    if (DUMMY_TYPE.equals(connectionTypeName)) {
      setConnectionType(ServiceLocation.DUMMY);
      return;
    }
    if (CICS_TYPE.equals(connectionTypeName)) {
      setConnectionType(ServiceLocation.CICS);
      return;
    }
    if (IMS_TYPE.equals(connectionTypeName)) {
      setConnectionType(ServiceLocation.IMS);
      return;
    }
    if (JDBC_TYPE.equals(connectionTypeName)) {
      setConnectionType(ServiceLocation.JDBC);
      return;
    }
    throw new LocationException(MESSAGES.getString("CIC001501_Invalid_type", connectionTypeName));
  }

}
