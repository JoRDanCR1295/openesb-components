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
 * @(#)MIMEConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.ibm.wsdl.extensions.mime;

import javax.xml.namespace.*;
import com.ibm.wsdl.*;

/**
 * @author Matthew J. Duftler (duftler@us.ibm.com)
 */
public class MIMEConstants
{
  // Namespace URIs.
  public static final String NS_URI_MIME =
    "http://schemas.xmlsoap.org/wsdl/mime/";

  // Element names.
  public static final String ELEM_CONTENT = "content";
  public static final String ELEM_MULTIPART_RELATED = "multipartRelated";
  public static final String ELEM_MIME_XML = "mimeXml";

  // Qualified element names.
  public static final QName Q_ELEM_MIME_CONTENT =
    new QName(NS_URI_MIME, ELEM_CONTENT);
  public static final QName Q_ELEM_MIME_MULTIPART_RELATED =
    new QName(NS_URI_MIME, ELEM_MULTIPART_RELATED);
  public static final QName Q_ELEM_MIME_PART =
    new QName(NS_URI_MIME, Constants.ELEM_PART);
  public static final QName Q_ELEM_MIME_MIME_XML =
    new QName(NS_URI_MIME, ELEM_MIME_XML);

  // Attribute names.
  public static final String ATTR_PART = "part";
}
