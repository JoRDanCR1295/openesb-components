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
 * @(#)MessageImpl.java 
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
 * This class describes a message used for communication with an operation.
 *
 * @author Paul Fremantle
 * @author Nirmal Mukhi
 * @author Matthew J. Duftler
 */
public class MessageImpl extends AbstractWSDLElement implements Message
{
  protected Map parts = new HashMap();
  protected List additionOrderOfParts = new Vector();
  protected QName name = null;
  protected List nativeAttributeNames =
    Arrays.asList(Constants.MESSAGE_ATTR_NAMES);
  protected boolean isUndefined = true;

  public static final long serialVersionUID = 1;

  /**
   * Set the name of this message.
   *
   * @param name the desired name
   */
  public void setQName(QName name)
  {
    this.name = name;
  }

  /**
   * Get the name of this message.
   *
   * @return the message name
   */
  public QName getQName()
  {
    return name;
  }

  /**
   * Add a part to this message.
   *
   * @param part the part to be added
   */
  public void addPart(Part part)
  {
    String partName = part.getName();

    parts.put(partName, part);
    additionOrderOfParts.add(partName);
  }

  /**
   * Get the specified part.
   *
   * @param name the name of the desired part.
   * @return the corresponding part, or null if there wasn't
   * any matching part
   */
  public Part getPart(String name)
  {
    return (Part)parts.get(name);
  }
  
  /**
   * Remove the specified part.
   *
   * @param name the name of the part to be removed.
   * @return the part which was removed
   */
  public Part removePart(String name)
  {
    return (Part)parts.remove(name);
  }

  /**
   * Get all the parts defined here.
   */
  public Map getParts()
  {
    return parts;
  }

  /**
   * Get an ordered list of parts as specified by the partOrder
   * argument.
   *
   * @param partOrder a list of strings, with each string referring
   * to a part by its name. If this argument is null, the parts are
   * returned in the order in which they were added to the message.
   * @return the list of parts
   */
  public List getOrderedParts(List partOrder)
  {
    List orderedParts = new Vector();

    if (partOrder == null)
    {
      partOrder = additionOrderOfParts;
    }

    Iterator partNameIterator = partOrder.iterator();

    while (partNameIterator.hasNext())
    {
      String partName = (String)partNameIterator.next();
      Part part = getPart(partName);

      if (part != null)
      {
        orderedParts.add(part);
      }
    }

    return orderedParts;
  }

  public void setUndefined(boolean isUndefined)
  {
    this.isUndefined = isUndefined;
  }

  public boolean isUndefined()
  {
    return isUndefined;
  }

  public String toString()
  {
    StringBuffer strBuf = new StringBuffer();

    strBuf.append("Message: name=" + name);

    if (parts != null)
    {
      Iterator partsIterator = parts.values().iterator();

      while (partsIterator.hasNext())
      {
        strBuf.append("\n" + partsIterator.next());
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
