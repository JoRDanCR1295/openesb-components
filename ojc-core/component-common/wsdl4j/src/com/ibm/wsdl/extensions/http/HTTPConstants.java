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
 * @(#)HTTPConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.ibm.wsdl.extensions.http;

import javax.xml.namespace.*;
import com.ibm.wsdl.*;

/**
 * @author Matthew J. Duftler (duftler@us.ibm.com)
 */
public class HTTPConstants
{
  // Namespace URIs.
  public static final String NS_URI_HTTP =
    "http://schemas.xmlsoap.org/wsdl/http/";

  // Element names.
  public static final String ELEM_ADDRESS = "address";
  public static final String ELEM_URL_ENCODED = "urlEncoded";
  public static final String ELEM_URL_REPLACEMENT = "urlReplacement";

  // Qualified element names.
  public static final QName Q_ELEM_HTTP_BINDING =
    new QName(NS_URI_HTTP, Constants.ELEM_BINDING);
  public static final QName Q_ELEM_HTTP_OPERATION =
    new QName(NS_URI_HTTP, Constants.ELEM_OPERATION);
  public static final QName Q_ELEM_HTTP_ADDRESS =
    new QName(NS_URI_HTTP, ELEM_ADDRESS);
  public static final QName Q_ELEM_HTTP_URL_ENCODED =
    new QName(NS_URI_HTTP, ELEM_URL_ENCODED);
  public static final QName Q_ELEM_HTTP_URL_REPLACEMENT =
    new QName(NS_URI_HTTP, ELEM_URL_REPLACEMENT);

  // Attribute names.
  public static final String ATTR_VERB = "verb";
}
