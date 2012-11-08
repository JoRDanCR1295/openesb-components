 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator.bcm;

import java.util.ArrayList;
import java.util.List;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.commons.EmptyVisitor;

/**
 * XXX javadoc.
 */
public class AnnotationCollectorVisitor extends EmptyVisitor {

  /**
   * Logger.
   */
  private static final Logger LOG = LoggerFactory.getLogger(
    AnnotationCollectorVisitor.class);

  
  /**
   * XXX javadoc.
   */
  public List<AnnotationValue> list = new ArrayList<AnnotationValue>();

  /**
   * Default constructor.
   */
  public AnnotationCollectorVisitor (){
  }
  
  /**
   * @param name   The name
   * @param value  The value
   */
  public void visit(String name, Object value) {
    LOG.debug("AnnotationCollectorVisitor.visit(String, Object)."
      + "name=" + name + "; value=" + value);

    AnnotationValue av = new AnnotationValue(AnnotationValue.ANNOTATION_VISIT);

    av.name  = name;
    av.value = value;

    list.add(av);

    super.visit(name, value);
  }

  /**
   * @param name  The name
   * @param desc  The description
   * @return      The return
   */
  public AnnotationVisitor visitAnnotation(String name, String desc) {
    LOG.debug("AnnotationCollectorVisitor.visitAnnotation(String, String)."
      + "name=" + name + "; desc=" + desc);

    AnnotationValue av = new AnnotationValue(
      AnnotationValue.ANNOTATION_VISIT_ANNOTATION);

    av.name = name;
    av.desc = desc;

    list.add(av);

    AnnotationCollectorVisitor annotationCollectorVisitor
      = new AnnotationCollectorVisitor();

    av.nestedAnnotationValue = annotationCollectorVisitor.list;
    LOG.debug("AnnotationValue." + av);

    return annotationCollectorVisitor;
  }

  /**
   * @param name The name
   * @return     The return
   */
  public AnnotationVisitor visitArray(String name) {
    LOG.debug("AnnotationCollectorVisitor.visitArray(String)."
      + "name=" + name);

    AnnotationValue av = new AnnotationValue(
      AnnotationValue.ANNOTATION_VISIT_ARRAY);

    av.name  = name;

    list.add(av);

    AnnotationCollectorVisitor annotationCollectorVisitor
      = new AnnotationCollectorVisitor();

    av.nestedAnnotationValue = annotationCollectorVisitor.list;
    LOG.debug("AnnotationValue." + av);

    return annotationCollectorVisitor;
  }

  /**
   * @param name   The name
   * @param desc   The description
   * @param value  The value
   */
  public void visitEnum(String name, String desc, String value) {
    LOG.debug("AnnotationCollectorVisitor.visitEnum(String,String,String)."
      + "name=" + name + "; desc=" + desc + "; value=" + value);

    AnnotationValue av = new AnnotationValue(
      AnnotationValue.ANNOTATION_VISIT_ENUM);

    av.name  = name;
    av.desc  = desc;
    av.value = value;

    list.add(av);

    super.visitEnum(name, desc, value);
  }

}
