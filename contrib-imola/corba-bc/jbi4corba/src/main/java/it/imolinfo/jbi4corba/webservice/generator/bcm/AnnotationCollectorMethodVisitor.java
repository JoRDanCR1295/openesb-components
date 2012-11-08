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
 * XXX javadoc.
 */
public class AnnotationCollectorMethodVisitor extends EmptyVisitor {

  /**
   * Logger.
   */
  private static final Logger LOG
    = LoggerFactory.getLogger(AnnotationCollectorMethodVisitor.class);

  protected String methodName = null;
  protected AnnotationsMaps tracer;

    /**
     * XXX javadoc.
     * 
     * @param tracer  The annotations maps
     * @param methodName  The method name
     */
    public AnnotationCollectorMethodVisitor(AnnotationsMaps tracer,
      String methodName) {

      super();

      this.methodName = methodName;
      this.tracer = (tracer == null) ? new AnnotationsMaps() : tracer;
    }

    /**
     * Override.
     * @param desc     The descriptor
     * @param visible  The visible
     * @return         The return
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

      if (tracer.annotationOnMethod.get(methodName) == null) {
        Map<String, AnnotationVisibleAndValue> annotations
          = new HashMap<String, AnnotationVisibleAndValue>();

        tracer.annotationOnMethod.put(methodName, annotations);
      }
      tracer.annotationOnMethod.get(methodName).put(desc, avv);

      return av;
    }

}
