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
 * @(#)PartImpl.java 
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
 * This class represents a message part and contains the part's
 * name, elementName, typeName, and any extensibility attributes.
 *
 * @author Paul Fremantle
 * @author Nirmal Mukhi
 * @author Matthew J. Duftler
 */
public class PartImpl extends AbstractWSDLElement implements Part
{
  protected String name = null;
  protected QName elementName = null;
  protected QName typeName = null;
  protected List nativeAttributeNames =
    Arrays.asList(Constants.PART_ATTR_NAMES);

  public static final long serialVersionUID = 1;

  /**
   * Set the name of this part.
   *
   * @param name the desired name
   */
  public void setName(String name)
  {
    this.name = name;
  }

  /**
   * Get the name of this part.
   *
   * @return the part name
   */
  public String getName()
  {
    return name;
  }

  public void setElementName(QName elementName)
  {
    this.elementName = elementName;
  }

  public QName getElementName()
  {
    return elementName;
  }

  public void setTypeName(QName typeName)
  {
    this.typeName = typeName;
  }

  public QName getTypeName()
  {
    return typeName;
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

  public String toString()
  {
    StringBuffer strBuf = new StringBuffer();

    strBuf.append("Part: name=" + name);

    if (elementName != null)
    {
      strBuf.append("\nelementName=" + elementName);
    }

    if (typeName != null)
    {
      strBuf.append("\ntypeName=" + typeName);
    }

    String superString = super.toString();
    if(!superString.equals(""))
    {
      strBuf.append("\n");
      strBuf.append(superString);
    }

    return strBuf.toString();
  }
}
