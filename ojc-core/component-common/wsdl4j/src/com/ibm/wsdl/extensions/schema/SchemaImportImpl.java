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
 * @(#)SchemaImportImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.ibm.wsdl.extensions.schema;

import javax.wsdl.extensions.schema.SchemaImport;

/**
 * @author Jeremy Hughes <hughesj@uk.ibm.com>
 */
public class SchemaImportImpl extends SchemaReferenceImpl implements SchemaImport
{
  public static final long serialVersionUID = 1;

  private String namespace = null;

  /**
   * @return Returns the namespace.
   */
  public String getNamespaceURI()
  {
    return this.namespace;
  }

  /**
   * @param namespace The namespace to set.
   */
  public void setNamespaceURI(String namespace)
  {
    this.namespace = namespace;
  }
}
