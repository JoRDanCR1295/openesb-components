 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator.bcm;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.builder.ReflectionToStringBuilder;


/**
 * This class is used to collect the annotations found during the visit of a
 * class.
 */
public class AnnotationsMaps {

  public Map<String, AnnotationVisibleAndValue> annotationOnClass
    = new HashMap<String, AnnotationVisibleAndValue>();

  public Map<String, Map<String, AnnotationVisibleAndValue>> annotationOnField
    = new HashMap<String, Map<String, AnnotationVisibleAndValue>>();

  public Map<String, Map<String, AnnotationVisibleAndValue>> annotationOnMethod
    = new HashMap<String, Map<String, AnnotationVisibleAndValue>>();

  /**
   * Default constructor.
   */
  public AnnotationsMaps () {
	// NOP
  }
  
  /**
   * @return  The return
   */
  public String toString() {
    return ReflectionToStringBuilder.toString(this);
  }
}
