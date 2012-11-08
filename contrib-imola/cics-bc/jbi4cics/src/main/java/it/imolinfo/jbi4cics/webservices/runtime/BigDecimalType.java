/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.webservices.runtime;

import java.math.BigDecimal;

import javax.xml.namespace.QName;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.codehaus.xfire.MessageContext;
import org.codehaus.xfire.aegis.MessageReader;
import org.codehaus.xfire.aegis.MessageWriter;
import org.codehaus.xfire.aegis.type.Type;

public class BigDecimalType extends Type {
  
  private static Log log = LogFactory.getLog(BigDecimalType.class);  

  public BigDecimalType() {
    setNillable(true);
    setSchemaType(new QName("http://www.w3.org/2001/XMLSchema","decimal"));
    setTypeClass(BigDecimal.class);
  }

  public Object readObject(MessageReader reader, MessageContext context) {
    String value = reader.getValue();
    log.debug("big decimal value: ["+value+"]");
    if (value==null || "".equals(value)){
      return null;
    }
    return new BigDecimal(value);
  }

  public void writeObject(Object object, MessageWriter writer, MessageContext context) {
    writer.writeValue(object.toString());
  }
}
