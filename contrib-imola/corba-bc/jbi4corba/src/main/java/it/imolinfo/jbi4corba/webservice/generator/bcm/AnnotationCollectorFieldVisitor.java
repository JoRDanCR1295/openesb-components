 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator.bcm;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;

import java.util.HashMap;
import java.util.Map;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.commons.EmptyVisitor;

/**
 * This class is used to collect the annotations on a field.
 */
public class AnnotationCollectorFieldVisitor extends EmptyVisitor {

  /**
   * Logger.
   */
  private static final Logger LOG
    = LoggerFactory.getLogger(AnnotationCollectorFieldVisitor.class);

  /** The field name. */
  protected String fieldName = null;

  /** Where the annotations are stored. */
  protected AnnotationsMaps tracer;

    /**
     * Constructor.
     *
     * @param tracer  The annotations maps
     * @param fieldName  The field name
     */
    public AnnotationCollectorFieldVisitor(AnnotationsMaps tracer,
      String fieldName) {

      super();

      this.fieldName = fieldName;
      this.tracer = (tracer == null) ? new AnnotationsMaps() : tracer;
    }

    /**
     * @param desc    The description
     * @param visible The visibol
     * @return        The return
     */
  @Override
  public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
	LOG.info("CRB000602_ANNOTATION", new Object[]{desc, visible});

    AnnotationCollectorVisitor visitor
        = new AnnotationCollectorVisitor();

    AnnotationCollectorVisitor av = (AnnotationCollectorVisitor)
        visitor.visitAnnotation(desc, visible);

    AnnotationVisibleAndValue avv
        = new AnnotationVisibleAndValue(visible, av.list);

    if (tracer.annotationOnField.get(fieldName) == null) {
        Map<String, AnnotationVisibleAndValue> annotations
        = new HashMap<String, AnnotationVisibleAndValue>();

        tracer.annotationOnField.put(fieldName, annotations);
    }
    tracer.annotationOnField.get(fieldName).put(desc, avv);

    return av;
  }

}
