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
 * @(#)SOAPConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.ibm.wsdl.extensions.soap;

import javax.xml.namespace.*;
import com.ibm.wsdl.*;

/**
 * @author Matthew J. Duftler (duftler@us.ibm.com)
 */
public class SOAPConstants
{
  // Namespace URIs.
  public static final String NS_URI_SOAP =
    "http://schemas.xmlsoap.org/wsdl/soap/";

  // Element names.
  public static final String ELEM_BODY = "body";
  public static final String ELEM_HEADER = "header";
  public static final String ELEM_HEADER_FAULT = "headerfault";
  public static final String ELEM_ADDRESS = "address";

  // Qualified element names.
  public static final QName Q_ELEM_SOAP_BINDING =
    new QName(NS_URI_SOAP, Constants.ELEM_BINDING);
  public static final QName Q_ELEM_SOAP_BODY =
    new QName(NS_URI_SOAP, ELEM_BODY);
  public static final QName Q_ELEM_SOAP_HEADER =
    new QName(NS_URI_SOAP, ELEM_HEADER);
  public static final QName Q_ELEM_SOAP_HEADER_FAULT =
    new QName(NS_URI_SOAP, ELEM_HEADER_FAULT);
  public static final QName Q_ELEM_SOAP_ADDRESS =
    new QName(NS_URI_SOAP, ELEM_ADDRESS);
  public static final QName Q_ELEM_SOAP_OPERATION =
    new QName(NS_URI_SOAP, Constants.ELEM_OPERATION);
  public static final QName Q_ELEM_SOAP_FAULT =
    new QName(NS_URI_SOAP, Constants.ELEM_FAULT);

  // Attribute names.
  public static final String ATTR_TRANSPORT = "transport";
  public static final String ATTR_STYLE = "style";
  public static final String ATTR_SOAP_ACTION = "soapAction";
  public static final String ATTR_PARTS = "parts";
  public static final String ATTR_USE = "use";
  public static final String ATTR_ENCODING_STYLE = "encodingStyle";
  public static final String ATTR_PART = "part";
}
