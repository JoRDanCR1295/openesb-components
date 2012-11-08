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
 * @(#)UnknownExtensibilityElement.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package javax.wsdl.extensions;

import org.w3c.dom.*;
import javax.xml.namespace.*;

/**
 * This class is used to wrap arbitrary elements.
 *
 * @see UnknownExtensionSerializer
 * @see UnknownExtensionDeserializer
 *
 * @author Matthew J. Duftler (duftler@us.ibm.com)
 */
public class UnknownExtensibilityElement implements ExtensibilityElement,
                                                    java.io.Serializable
{
  protected QName elementType = null;
  // Uses the wrapper type so we can tell if it was set or not.
  protected Boolean required = null;
  protected Element element = null;

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
   * Set the Element for this extensibility element.
   *
   * @param element the unknown element that was encountered
   */
  public void setElement(Element element)
  {
    this.element = element;
  }

  /**
   * Get the Element for this extensibility element.
   *
   * @return the unknown element that was encountered
   */
  public Element getElement()
  {
    return element;
  }

  public String toString()
  {
    StringBuffer strBuf = new StringBuffer();

    strBuf.append("UnknownExtensibilityElement (" + elementType + "):");
    strBuf.append("\nrequired=" + required);

    if (element != null)
    {
      strBuf.append("\nelement=" + element);
    }

    return strBuf.toString();
  }
}
