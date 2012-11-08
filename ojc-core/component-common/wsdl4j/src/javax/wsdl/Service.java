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
 * @(#)Service.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package javax.wsdl;

import java.util.*;
import javax.xml.namespace.*;

/**
 * This interface represents a service, which groups related
 * ports to provide some functionality.
 *
 * @author Paul Fremantle
 * @author Nirmal Mukhi
 * @author Matthew J. Duftler
 */
public interface Service extends WSDLElement
{
  /**
   * Set the name of this service.
   *
   * @param name the desired name
   */
  public void setQName(QName name);

  /**
   * Get the name of this service.
   *
   * @return the service name
   */
  public QName getQName();

  /**
   * Add a port to this service.
   *
   * @param port the port to be added
   */
  public void addPort(Port port);

  /**
   * Get the specified port.
   *
   * @param name the name of the desired port.
   * @return the corresponding port, or null if there wasn't
   * any matching port
   */
  public Port getPort(String name);
  
  /**
   * Remove the specified port.
   *
   * @param name the name of the port to be removed.
   * @return the port which was removed.
   */
  public Port removePort(String name);

  /**
   * Get all the ports defined here.
   */
  public Map getPorts();
}
