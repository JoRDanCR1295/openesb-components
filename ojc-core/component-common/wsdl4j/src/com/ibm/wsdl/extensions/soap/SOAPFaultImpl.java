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
 * @(#)SOAPFaultImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.ibm.wsdl.extensions.soap;

import java.util.*;
import javax.wsdl.extensions.soap.*;
import javax.xml.namespace.*;

/**
 * @author Matthew J. Duftler (duftler@us.ibm.com)
 */
public class SOAPFaultImpl implements SOAPFault
{
  protected QName elementType = SOAPConstants.Q_ELEM_SOAP_FAULT;
  protected Boolean required = null;
  protected String name = null;
  protected String use = null;
  protected List encodingStyles = null;
  protected String namespaceURI = null;

  public static final long serialVersionUID = 1;

  /**
   * Set the type of this extensibility element.
   *
   * @param elementType the type
   */
  public void setElementType(QName elementType)
  {
    this.elementType = elementType;
  }

  /**
   * Get the type of this extensibility element.
   *
   * @return the extensibility element's type
   */
  public QName getElementType()
  {
    return elementType;
  }

  /**
   * Set whether or not the semantics of this extension
   * are required. Relates to the wsdl:required attribute.
   */
  public void setRequired(Boolean required)
  {
    this.required = required;
  }

  /**
   * Get whether or not the semantics of this extension
   * are required. Relates to the wsdl:required attribute.
   */
  public Boolean getRequired()
  {
    return required;
  }

  /**
   * Set the name for this SOAP fault.
   *
   * @param name the desired name
   */
  public void setName(String name)
  {
    this.name = name;
  }

  /**
   * Get the name for this SOAP fault.
   */
  public String getName()
  {
    return name;
  }

  /**
   * Set the use for this SOAP fault.
   *
   * @param use the desired use
   */
  public void setUse(String use)
  {
    this.use = use;
  }

  /**
   * Get the use for this SOAP fault.
   */
  public String getUse()
  {
    return use;
  }

  /**
   * Set the encodingStyles for this SOAP fault.
   *
   * @param encodingStyles the desired encodingStyles
   */
  public void setEncodingStyles(List encodingStyles)
  {
    this.encodingStyles = encodingStyles;
  }

  /**
   * Get the encodingStyles for this SOAP fault.
   */
  public List getEncodingStyles()
  {
    return encodingStyles;
  }

  /**
   * Set the namespace URI for this SOAP fault.
   *
   * @param namespaceURI the desired namespace URI
   */
  public void setNamespaceURI(String namespaceURI)
  {
    this.namespaceURI = namespaceURI;
  }

  /**
   * Get the namespace URI for this SOAP fault.
   */
  public String getNamespaceURI()
  {
    return namespaceURI;
  }

  public String toString()
  {
    StringBuffer strBuf = new StringBuffer();

    strBuf.append("SOAPFault (" + elementType + "):");
    strBuf.append("\nrequired=" + required);

    if (name != null)
    {
      strBuf.append("\nname=" + name);
    }

    if (use != null)
    {
      strBuf.append("\nuse=" + use);
    }

    if (encodingStyles != null)
    {
      strBuf.append("\nencodingStyles=" + encodingStyles);
    }

    if (namespaceURI != null)
    {
      strBuf.append("\nnamespaceURI=" + namespaceURI);
    }

    return strBuf.toString();
  }
}
