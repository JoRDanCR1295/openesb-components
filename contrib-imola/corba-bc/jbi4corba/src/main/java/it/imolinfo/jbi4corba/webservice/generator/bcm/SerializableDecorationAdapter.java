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

import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

/**
 * This adapter makes a class serializable and add a serial version UID.
 */
public class SerializableDecorationAdapter
  extends SerializableInspectorAdapter {

  /**
   * Logger.
   */
  private static final Logger LOG
    = LoggerFactory.getLogger(SerializableDecorationAdapter.class);

  /**
   * The new serial version UID for the class inspected.
   */
  protected Long newSerialVersionUid = 0L;

  /**
   * Constructor.
   *
   * @param        cv                        The class visitor instance.
   * @param        aNewSerialVersionUid      The new serial version UID.
   */
  public SerializableDecorationAdapter(ClassVisitor cv,
                                       Long aNewSerialVersionUid) {
    super(cv);
    newSerialVersionUid = aNewSerialVersionUid != null ? aNewSerialVersionUid
                                                       : new Long(0L);
  }

  /**
   * Override.
   * @param version     The version
   * @param access      The access
   * @param name        The name
   * @param signature   The signature
   * @param superName   The super name
   * @param interfaces  The interfaces
   */
  @Override
  public void visit(int version, int access, String name, String signature,
    String superName, String [] interfaces) {

    String [] implementsArray = interfaces;

    if (! implementsSerializable(interfaces)) {
      LOG.debug("The class " + name + " does not implement Serializable.");

      int implementsSize = interfaces == null ? 0 : interfaces.length;

      implementsArray = new String[implementsSize + 1];

      if (implementsSize == 0 ) {

        implementsArray[0]
          = SerializableInspectorAdapter.INTERNAL_NAME_OF_SERIALIZABLE;

      } else {

        System.arraycopy(interfaces, 0, implementsArray, 0, implementsSize);
        implementsArray[implementsSize]
          = SerializableInspectorAdapter.INTERNAL_NAME_OF_SERIALIZABLE;

      }

    }

    super.visit(version, access, name, signature, superName, implementsArray);
  }

  /**
   * Override.
   * @param access     The access
   * @param name       The name
   * @param desc       The description
   * @param signature  The signature
   * @param value      The value
   * @return           The return
   */
  @Override
  public FieldVisitor visitField(int access, String name, String desc,
    String signature, Object value) {

    LOG.debug("visitField. access=" + access + "; name=" + name
      + "; desc=" + desc + "; signature=" + signature + "; value=" + value);

    if (hasSerialVersionUIDField(name, desc)) {
      LOG.debug("The class " + name + " has a serial version UID:" + value);

      value = newSerialVersionUid;
    }

    return super.visitField(access, name, desc, signature, value);
  }

  /**
   * Override.
   */
  @Override
  public void visitEnd() {
    if (getClassMetaInfo().getClassSerialVersionUid() == null) {
      super.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC,
        SerializableInspectorAdapter.FIELDNAME_SERIAL_VERSION_UID,
        Type.LONG_TYPE.getDescriptor(),
        null,
        newSerialVersionUid);

      getClassMetaInfo().setClassSerialVersionUid(newSerialVersionUid);
    }
  }
}
