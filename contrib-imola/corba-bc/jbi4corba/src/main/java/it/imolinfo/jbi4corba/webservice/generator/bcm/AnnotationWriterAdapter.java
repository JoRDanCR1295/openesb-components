 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator.bcm;

import java.util.List;
import java.util.Map;

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
public class AnnotationWriterAdapter extends ClassAdapter {

  /**
   * Logger.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(AnnotationWriterAdapter.class);

//HAS A ...
  public AnnotationsMaps tracer;
  
  /**
   * The Class Writer.
   */
  protected ClassWriter classWriter = null;

  /**
   * The class name.
   */
  protected String className = null;

  

  /**
   * Constructor.
   *
   * @param    cv        The class visitor
   * @param    tracer    The tracer
   */
  public AnnotationWriterAdapter(ClassVisitor cv, AnnotationsMaps tracer) {
    super(cv);

    this.tracer = tracer;
  }

  /**
   * Override.
   * @param version    The version
   * @param access     The access
   * @param name       The name
   * @param signature  The signature
   * @param superName  The super name
   * @param interfaces The interfaces
   */
  @Override
  public void visit(int version,
                    int access,
                    String name,
                    String signature,
                    String superName,
                    String [] interfaces) {

    super.visit(version, access, name, signature, superName, interfaces);

    // <desc, (visible, value)>
    Map<String, AnnotationVisibleAndValue> ann = tracer.annotationOnClass;

    if (ann == null) {
      LOG.debug("visit. NO ANNOTATIONS FOR " + name);
      return;
    }
    //else
    for (String annDesc : ann.keySet()) {
      AnnotationVisibleAndValue annotationVisibleAndValue = ann.get(annDesc);

      if (annotationVisibleAndValue == null) {
        LOG.debug("skip ... " + annDesc);
        continue;
      } else {
        LOG.debug("Annotation. desc=" + annDesc + "; visible="
          + annotationVisibleAndValue.visible);
      }

      AnnotationVisitor annVisitor
        = super.visitAnnotation(annDesc, annotationVisibleAndValue.visible);

      writeAnnotationValue(annVisitor, annotationVisibleAndValue.value);
      annVisitor.visitEnd();
    }

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
    MethodVisitor methodVisitor
      = super.visitMethod(access, name, desc, signature, exceptions);

    Map<String, AnnotationVisibleAndValue> ann
      = tracer.annotationOnMethod.get(name);

    if (ann == null) {
      LOG.debug("visitMethod. NO ANNOTATIONS FOR " + name);
      return methodVisitor;
    }

    // else

    for (String annName : ann.keySet()) {
      AnnotationVisibleAndValue annotationVisibleAndValue = ann.get(annName);

      if (annotationVisibleAndValue == null) {
    	  continue;
      }
      //else
      AnnotationVisitor annVisitor = methodVisitor.visitAnnotation(annName,
        annotationVisibleAndValue.visible);

      writeAnnotationValue(annVisitor, annotationVisibleAndValue.value);
      annVisitor.visitEnd();
    }

    return methodVisitor;
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
    FieldVisitor fieldVisitor
      = super.visitField(access, name, desc, signature, value);

    Map<String, AnnotationVisibleAndValue> ann
      = tracer.annotationOnField.get(name);

    if (ann == null) {
      LOG.debug("visitField. NO ANNOTATIONS FOR " + name);
      return fieldVisitor;
    }

    // else

    for (String annName : ann.keySet()) {
      AnnotationVisibleAndValue annotationVisibleAndValue = ann.get(annName);

      if (annotationVisibleAndValue == null) {
        LOG.debug("skip ... " + annName);
        continue;
      }
      //else
      AnnotationVisitor annVisitor = fieldVisitor.visitAnnotation(annName,
        annotationVisibleAndValue.visible);

      writeAnnotationValue(annVisitor, annotationVisibleAndValue.value);
      annVisitor.visitEnd();
    }

    return fieldVisitor;
  }


/**
 * @param visitor    The visitor
 * @param valueList  The value list
 */
  private void writeAnnotationValue(
    AnnotationVisitor visitor, List<AnnotationValue> valueList) {

    if (valueList == null) {
        LOG.debug("No annotation value.");
        return;
    }

    for (AnnotationValue annValue : valueList) {

      LOG.debug("AnnotationValue["+ annValue.type + "]. name=" + annValue.name
        + "; desc=" + annValue.desc + "; value=" + annValue.value);

      switch (annValue.type) {
      case AnnotationValue.ANNOTATION_VISIT:
          visitor.visit(annValue.name, annValue.value);
          break;
      case AnnotationValue.ANNOTATION_VISIT_ANNOTATION:
        AnnotationVisitor visitorNested
          = visitor.visitAnnotation(annValue.name, annValue.desc);

        writeAnnotationValue(visitorNested, annValue.nestedAnnotationValue);
        break;
      case AnnotationValue.ANNOTATION_VISIT_ARRAY:
          AnnotationVisitor visitorArray = visitor.visitArray(annValue.name);

          writeAnnotationValue(visitorArray, annValue.nestedAnnotationValue);
          visitorArray.visitEnd();
          break;
      case AnnotationValue.ANNOTATION_VISIT_ENUM:
          visitor.visitEnum(annValue.name, annValue.desc, (String) annValue.value);
          break;
      }

    }
  }

}
