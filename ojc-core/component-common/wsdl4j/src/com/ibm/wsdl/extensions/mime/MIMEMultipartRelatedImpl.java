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
 * @(#)MIMEMultipartRelatedImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.ibm.wsdl.extensions.mime;

import java.util.*;
import javax.wsdl.extensions.mime.*;
import javax.xml.namespace.*;

/**
 * @author Matthew J. Duftler (duftler@us.ibm.com)
 */
public class MIMEMultipartRelatedImpl implements MIMEMultipartRelated
{
  protected QName elementType = MIMEConstants.Q_ELEM_MIME_MULTIPART_RELATED;
  // Uses the wrapper type so we can tell if it was set or not.
  protected Boolean required = null;
  protected List mimeParts = new Vector();

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
   * Add a MIME part to this MIME multipart related.
   *
   * @param mimePart the MIME part to be added
   */
  public void addMIMEPart(MIMEPart mimePart)
  {
    mimeParts.add(mimePart);
  }
  
  /**
   * Remove a MIME part from this MIME multipart related.
   *
   * @param mimePart the MIME part to be removed.
   * @return the MIME part which was removed.
   */
  public MIMEPart removeMIMEPart(MIMEPart mimePart)
  {
    if(mimeParts.remove(mimePart))
      return mimePart;
    else
      return null;
  }

  /**
   * Get all the MIME parts defined here.
   */
  public List getMIMEParts()
  {
    return mimeParts;
  }

  public String toString()
  {
    StringBuffer strBuf = new StringBuffer();

    strBuf.append("MIMEMultipartRelated (" + elementType + "):");
    strBuf.append("\nrequired=" + required);

    if (mimeParts != null)
    {
      Iterator mimePartIterator = mimeParts.iterator();

      while (mimePartIterator.hasNext())
      {
        strBuf.append("\n" + mimePartIterator.next());
      }
    }

    return strBuf.toString();
  }
}
