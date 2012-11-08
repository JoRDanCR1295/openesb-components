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
 * @(#)ServiceImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.ibm.wsdl;

import java.util.*;

import javax.wsdl.*;
import javax.xml.namespace.*;

/**
 * This class represents a service, which groups related
 * ports to provide some functionality.
 *
 * @author Paul Fremantle
 * @author Nirmal Mukhi
 * @author Matthew J. Duftler
 */
public class ServiceImpl extends AbstractWSDLElement implements Service
{
  protected QName name = null;
  protected Map ports = new HashMap();
  protected List nativeAttributeNames =
    Arrays.asList(Constants.SERVICE_ATTR_NAMES);

  public static final long serialVersionUID = 1;

  /**
   * Set the name of this service.
   *
   * @param name the desired name
   */
  public void setQName(QName name)
  {
    this.name = name;
  }

  /**
   * Get the name of this service.
   *
   * @return the service name
   */
  public QName getQName()
  {
    return name;
  }

  /**
   * Add a port to this service.
   *
   * @param port the port to be added
   */
  public void addPort(Port port)
  {
    ports.put(port.getName(), port);
  }

  /**
   * Get the specified port.
   *
   * @param name the name of the desired port.
   * @return the corresponding port, or null if there wasn't
   * any matching port
   */
  public Port getPort(String name)
  {
    return (Port)ports.get(name);
  }
  
  /**
   * Remove the specified port.
   *
   * @param name the name of the port to be removed.
   * @return the port which was removed
   */
  public Port removePort(String name)
  {
    return (Port)ports.remove(name);
  }

  /**
   * Get all the ports defined here.
   */
  public Map getPorts()
  {
    return ports;
  }

  public String toString()
  {
    StringBuffer strBuf = new StringBuffer();

    strBuf.append("Service: name=" + name);

    if (ports != null)
    {
      Iterator portIterator = ports.values().iterator();

      while (portIterator.hasNext())
      {
        strBuf.append("\n" + portIterator.next());
      }
    }

    String superString = super.toString();
    if(!superString.equals(""))
    {
      strBuf.append("\n");
      strBuf.append(superString);
    }

    return strBuf.toString();
  }
  
  /**
   * Get the list of local attribute names defined for this element in
   * the WSDL specification.
   *
   * @return a List of Strings, one for each local attribute name
   */
  public List getNativeAttributeNames()
  {
    return nativeAttributeNames;
  }
}
