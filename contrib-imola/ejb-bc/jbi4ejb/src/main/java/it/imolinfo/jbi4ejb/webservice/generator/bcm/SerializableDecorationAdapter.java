/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.webservice.generator.bcm;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;

import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

/**
 * This adapter makes a class Serializable and add a serial version UID.
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
  private Long newSerialVersionUid = 0L;

  /**
   * SerializableDecorationAdapter Constructor.
   *
   * @param        cv                        The class visitor instance.
   * @param        aNewSerialVersionUid      The serialVersionUID value to add.
   */
  public SerializableDecorationAdapter(ClassVisitor cv,
                                       Long aNewSerialVersionUid) {
    super(cv);
    if (aNewSerialVersionUid != null) {
        newSerialVersionUid = aNewSerialVersionUid;
    } else {
        newSerialVersionUid = Long.valueOf(0L);
    }    
  }
   
  /**
     * Class visitro, adds the <code>java.io.Serializable</code> interface if not already present.
     * 
     * @param version
     *          The class version
     * @param access
     *          The access modifier
     * @param name
     *          The class name
     * @param signature
     *          The signature
     * @param superName
     *          The superclass name
     * @param interfaces
     *          The interfaces implemented
     */
  public void visit(int version, int access, String name, String signature,
    String superName, String [] interfaces) {

    String [] implementsArray = interfaces;

    if (! implementsSerializable(interfaces)) {
      LOG.debug("The class " + name + " does not implement Serializable.");

      int implementsSize = 0;
      if (interfaces != null) {
          implementsSize = interfaces.length;
      }

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
     * Field Visitor. Test if the field is a serialVersionUID field and adds the
     * newSerialVersionUid.
     * 
     * @param access
     *          The access modifier
     * @param name
     *          The field name
     * @param desc
     *          The field desc
     * @param signature
     *          The signature
     * @param value
     *          The field value
     * 
     * @return
     *          The FieldVisitor
     */
  public FieldVisitor visitField(int access, String name, String desc,
    String signature, Object value) {

    Object retValue = value; 
    LOG.debug("visitField. access=" + access + "; name=" + name
      + "; desc=" + desc + "; signature=" + signature + "; value=" + value);

    if (hasSerialVersionUIDField(name, desc)) {
      LOG.debug("The class " + name + " has a serial version UID:" + value);

      retValue = newSerialVersionUid;
    }

    return super.visitField(access, name, desc, signature, retValue);
  }


  /**
   * Adds the 'private final static' access modifier to the serialVersionUID field. 
   * @see org.objectweb.asm.ClassAdapter#visitEnd()
   */
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
