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

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;

/**
 * XXX javadoc.
 */
public class AnnotationCollectorAdapter extends ClassAdapter {

  /**
   * Logger.
   */
  private static final Logger LOG = LoggerFactory.getLogger(
    AnnotationCollectorAdapter.class);

  /**
   * The Class Writer.
   */
  protected ClassWriter classWriter = null;

  /**
   * The class name.
   */
  protected String className = null;

  /**
   * The java class name.
   */
  protected String javaClassName = null;

  protected AnnotationsMaps tracer = null;


  /**
   * Constructor.
   *
   * @param    cv        The class visitor
   * @param    cw        The class writer
   * @param    cn        The class name
   * @param    tracer    The annotations map
   * @param    javaClassName  The java class name
   */
  public AnnotationCollectorAdapter(ClassVisitor cv, ClassWriter cw, String cn,
    AnnotationsMaps tracer, String javaClassName) {

    super(cv);
    classWriter = cw;
    className = cn;

    this.javaClassName = javaClassName;

    this.tracer = (tracer == null) ? new AnnotationsMaps() : tracer;
  }

  /**
   * Override.
   * @param desc     The description
   * @param visible  The visible
   * @return         The return
   */
  @Override
  public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
	  LOG.info("CRB000602_ANNOTATION", new Object[]{desc, visible});

    AnnotationCollectorVisitor visitor
      = new AnnotationCollectorVisitor();

    //return super.visitAnnotation(desc, visible);
    //return visitor.visitAnnotation(desc, visible);

    AnnotationCollectorVisitor av = (AnnotationCollectorVisitor)
      visitor.visitAnnotation(desc, visible);

    AnnotationVisibleAndValue avv
      = new AnnotationVisibleAndValue(visible, av.list);

    tracer.annotationOnClass.put(desc, avv);

    return av;
  }

  /**
   * Override.
   * @param access      The access
   * @param name        The name
   * @param desc        The description
   * @param signature   The signature
   * @param exceptions  The exceptions
   * @return            The return
   */
  @Override
  public MethodVisitor visitMethod(int access,
          String name,
          String desc,
          String signature,
          String[] exceptions) {

    AnnotationCollectorMethodVisitor mv
      = new AnnotationCollectorMethodVisitor(tracer, name);

    return mv.visitMethod(access, name, desc, signature, exceptions);
  }

  /**
   * Override.
   * @param access      The access
   * @param name        The name
   * @param desc        The description
   * @param signature   The signature
   * @param value       The value
   * @return            The return
   */
  @Override
  public FieldVisitor visitField(int access,
                                 String name,
                                 String desc,
                                 String signature,
                                 Object value) {
      LOG.debug(">>>>> visitField - begin");

      AnnotationCollectorFieldVisitor fv
        = new AnnotationCollectorFieldVisitor(tracer, name);

      LOG.debug(">>>>> visitField - end");
      return fv.visitField(access, name, desc, signature, value);
  }

}
