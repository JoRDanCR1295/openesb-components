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
 * @(#)SchemaReferenceImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.ibm.wsdl.extensions.schema;

import javax.wsdl.extensions.schema.Schema;
import javax.wsdl.extensions.schema.SchemaReference;

/**
 * @author Jeremy Hughes <hughesj@uk.ibm.com>
 */
public class SchemaReferenceImpl implements SchemaReference
{

  public static final long serialVersionUID = 1;

  private String id = null;

  private String schemaLocation = null;

  private Schema referencedSchema = null;

  /**
   * @return Returns the id.
   */
  public String getId()
  {
    return this.id;
  }

  /**
   * @param id The id to set.
   */
  public void setId(String id)
  {
    this.id = id;
  }

  /**
   * @return Returns the schemaLocation.
   */
  public String getSchemaLocationURI()
  {
    return this.schemaLocation;
  }

  /**
   * @param schemaLocation The schemaLocation to set.
   */
  public void setSchemaLocationURI(String schemaLocation)
  {
    this.schemaLocation = schemaLocation;
  }

  /**
   * @return Returns the importedSchema.
   */
  public Schema getReferencedSchema()
  {
    return this.referencedSchema;
  }

  /**
   * @param referencedSchema The importedSchema to set.
   */
  public void setReferencedSchema(Schema referencedSchema)
  {
    this.referencedSchema = referencedSchema;
  }
}
