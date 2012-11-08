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
 * @(#)SOAPFaultSerializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.ibm.wsdl.extensions.soap;

import java.io.*;
import org.w3c.dom.*;
import javax.wsdl.*;
import javax.wsdl.extensions.*;
import javax.wsdl.extensions.soap.*;
import javax.xml.namespace.*;
import com.ibm.wsdl.*;
import com.ibm.wsdl.util.*;
import com.ibm.wsdl.util.xml.*;

/**
 * @author Matthew J. Duftler (duftler@us.ibm.com)
 */
public class SOAPFaultSerializer implements ExtensionSerializer,
                                            ExtensionDeserializer,
                                            Serializable
{
  public static final long serialVersionUID = 1;

  public void marshall(Class parentType,
                       QName elementType,
                       ExtensibilityElement extension,
                       PrintWriter pw,
                       Definition def,
                       ExtensionRegistry extReg)
                         throws WSDLException
  {
    SOAPFault soapFault = (SOAPFault)extension;

    if (soapFault != null)
    {
      String tagName =
        DOMUtils.getQualifiedValue(SOAPConstants.NS_URI_SOAP,
                                   "fault",
                                   def);

      pw.print("        <" + tagName);

      DOMUtils.printAttribute(Constants.ATTR_NAME, soapFault.getName(), pw);
      DOMUtils.printAttribute(SOAPConstants.ATTR_USE, soapFault.getUse(), pw);
      DOMUtils.printAttribute(SOAPConstants.ATTR_ENCODING_STYLE,
                      StringUtils.getNMTokens(soapFault.getEncodingStyles()),
                      pw);
      DOMUtils.printAttribute(Constants.ATTR_NAMESPACE,
                              soapFault.getNamespaceURI(),
                              pw);

      Boolean required = soapFault.getRequired();

      if (required != null)
      {
        DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                                         required.toString(),
                                         def,
                                         pw);
      }

      pw.println("/>");
    }
  }

  public ExtensibilityElement unmarshall(Class parentType,
                                         QName elementType,
                                         Element el,
                                         Definition def,
                                         ExtensionRegistry extReg)
                                           throws WSDLException
  {
    SOAPFault soapFault = (SOAPFault)extReg.createExtension(parentType,
                                                            elementType);
    //TODO: remove unused variable, message
    QName message = DOMUtils.getQualifiedAttributeValue(el,
                                                    Constants.ATTR_MESSAGE,
                                                    SOAPConstants.ELEM_HEADER,
                                                    false,
                                                    def);
    String name = DOMUtils.getAttribute(el, Constants.ATTR_NAME);
    String use = DOMUtils.getAttribute(el, SOAPConstants.ATTR_USE);
    String encStyleStr = DOMUtils.getAttribute(el,
                                          SOAPConstants.ATTR_ENCODING_STYLE);
    String namespaceURI = DOMUtils.getAttribute(el,
                                                Constants.ATTR_NAMESPACE);
    String requiredStr = DOMUtils.getAttributeNS(el,
                                                 Constants.NS_URI_WSDL,
                                                 Constants.ATTR_REQUIRED);

    if (name != null)
    {
      soapFault.setName(name);
    }

    if (use != null)
    {
      soapFault.setUse(use);
    }

    if (encStyleStr != null)
    {
      soapFault.setEncodingStyles(StringUtils.parseNMTokens(encStyleStr));
    }

    if (namespaceURI != null)
    {
      soapFault.setNamespaceURI(namespaceURI);
    }

    if (requiredStr != null)
    {
      soapFault.setRequired(new Boolean(requiredStr));
    }

    return soapFault;
  }
}
