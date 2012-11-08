 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator.bcm;

import java.util.HashSet;
import java.util.Set;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.webservice.generator.ClassMetaInfo;

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Type;

/**
 * This adapter inspect a class to find Serializable interface and serial
 * version UID.
 */
public class SerializableInspectorAdapter extends ClassAdapter {

  
  /**
   * The internal name of the java interface 'Serializable'.
   */
  public static final String INTERNAL_NAME_OF_SERIALIZABLE
    = "java/io/Serializable";

  /**
   * The field 'serialVersionUID'.
   */
  public static final String FIELDNAME_SERIAL_VERSION_UID = "serialVersionUID";

  /**
   * Logger.
   */
  private static final Logger LOG
    = LoggerFactory.getLogger(SerializableInspectorAdapter.class);

  
  /**
   * The meta information of the class inspected.
   */
  protected ClassMetaInfo classMetaInfo = new ClassMetaInfo();

  /**
   * Constructor.
   *
   * @param        cv        The class visitor instance.
   */
  public SerializableInspectorAdapter(ClassVisitor cv) {
    super(cv);
  }
  
  /**
   * Override.
   * @param version     The version
   * @param access      The access
   * @param name        The name
   * @param signature   The signature
   * @param superName   The superName
   * @param interfaces  The interfaces
   */
  @Override
  public void visit(int version, int access, String name, String signature,
    String superName, String [] interfaces) {

    LOG.debug("visit. version=" + version + "; access=" + access
      + "; name" + name + "; superName=" + superName);

    if (implementsSerializable(interfaces)) {
      LOG.debug("The class " + name + " implements Serializable.");
      classMetaInfo.setSerializable(true);

    } else {
      LOG.debug("The class " + name + " does not implement Serializable.");
      classMetaInfo.setSerializable(false);
    }

    classMetaInfo.setClassName(name.replace('/', '.'));
    classMetaInfo.setSuperClassName(superName.replace('/', '.'));

    Set<String> set = new HashSet<String>();
    if (interfaces != null) {
      for (String current : interfaces) {
        set.add(current.replace('/', '.'));
      }
    }
    classMetaInfo.setInterfaces(set);

    super.visit(version, access, name, signature, superName, interfaces);
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
  public FieldVisitor visitField(int access, String name, String desc,
    String signature, Object value) {

    LOG.debug("visitField. access=" + access + "; name=" + name
      + "; desc=" + desc + "; signature=" + signature + "; value=" + value);

    if (hasSerialVersionUIDField(name, desc)) {
      LOG.debug("The class " + name + " has a serial version UID:" + value);

      classMetaInfo.setClassSerialVersionUid((Long) value);
    }

    return super.visitField(access, name, desc, signature, value);
  }

  /**
   * Utility.
   * @param name  The name
   * @param desc  The description
   * @return      The return
   */
  //Utility
  protected boolean hasSerialVersionUIDField(String name, String desc) {
    if (FIELDNAME_SERIAL_VERSION_UID.equals(name)
        && Type.LONG_TYPE.getDescriptor().equals(desc)) {

      LOG.debug(FIELDNAME_SERIAL_VERSION_UID + " found.");
      return true;
    }
    // else
    return false;
  }

  /**
   * @param interfaces  The interfaces
   * @return            The return
   */
  protected boolean implementsSerializable(String [] interfaces) {
    if (interfaces == null || interfaces.length == 0) {
      return false;
    }
    // else
    for (String current : interfaces) {
      if (INTERNAL_NAME_OF_SERIALIZABLE.equals(current)) {
        return true;
      }
    }
    // else
    return false;
  }

  /**
   * @return            The return
   */
  public ClassMetaInfo getClassMetaInfo() {
    return classMetaInfo;
  }

  /**
   * @param classMetaInfo  The class meta info
   */
  public void setClassMetaInfo(ClassMetaInfo classMetaInfo) {
    this.classMetaInfo = classMetaInfo;
  }

}
