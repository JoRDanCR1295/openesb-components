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
 * @(#)SOAPHeaderFaultImpl.java 
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
public class SOAPHeaderFaultImpl implements SOAPHeaderFault
{
  protected QName elementType = SOAPConstants.Q_ELEM_SOAP_HEADER_FAULT;
  protected Boolean required = null;
  protected QName message = null;
  protected String part = null;
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
   * Set the message for this SOAP header fault.
   *
   * @param message the desired message
   */
  public void setMessage(QName message)
  {
    this.message = message;
  }

  /**
   * Get the message for this SOAP header fault.
   */
  public QName getMessage()
  {
    return message;
  }

  /**
   * Set the part for this SOAP header fault.
   *
   * @param part the desired part
   */
  public void setPart(String part)
  {
    this.part = part;
  }

  /**
   * Get the part for this SOAP header fault.
   */
  public String getPart()
  {
    return part;
  }

  /**
   * Set the use for this SOAP header fault.
   *
   * @param use the desired use
   */
  public void setUse(String use)
  {
    this.use = use;
  }

  /**
   * Get the use for this SOAP header fault.
   */
  public String getUse()
  {
    return use;
  }

  /**
   * Set the encodingStyles for this SOAP header fault.
   *
   * @param encodingStyles the desired encodingStyles
   */
  public void setEncodingStyles(List encodingStyles)
  {
    this.encodingStyles = encodingStyles;
  }

  /**
   * Get the encodingStyles for this SOAP header fault.
   */
  public List getEncodingStyles()
  {
    return encodingStyles;
  }

  /**
   * Set the namespace URI for this SOAP header fault.
   *
   * @param namespaceURI the desired namespace URI
   */
  public void setNamespaceURI(String namespaceURI)
  {
    this.namespaceURI = namespaceURI;
  }

  /**
   * Get the namespace URI for this SOAP header fault.
   */
  public String getNamespaceURI()
  {
    return namespaceURI;
  }

  public String toString()
  {
    StringBuffer strBuf = new StringBuffer();

    strBuf.append("SOAPHeaderFault (" + elementType + "):");
    strBuf.append("\nrequired=" + required);

    if (message != null)
    {
      strBuf.append("\nmessage=" + message);
    }

    if (part != null)
    {
      strBuf.append("\npart=" + part);
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