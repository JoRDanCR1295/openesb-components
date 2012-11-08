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
import java.util.ArrayList;
import java.util.List;


import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Opcodes;

/**
 * Used to find a class that provides a corba enum implementation.
 */
public class CorbaEnumAdapter extends ClassAdapter {

  /** Logger. */
  private static final Logger LOG
          = LoggerFactory.getLogger(CorbaEnumAdapter.class);

  /** The internal class name. */
  private String internalClassName = null;

  /** true when the class inspected is a corba enum (not a enum class). */
  private boolean corbaEnum = false;

  /** The list of the labels of the enum. */
  private List<String> enumLabelList = new ArrayList<String>();

  /**
   * The adapater used to manipulate the code.
   *
   * @param   cv    The ClassVisitor used in this object.
   *
   */
  public CorbaEnumAdapter(ClassVisitor cv) {
    super(cv);

    LOG.debug("new CorbaEnumAdapter; ClassVisitor=" + cv);
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
  public void visit(int version,
                    int access,
                    String name,
                    String signature,
                    String superName,
                    String [] interfaces) {
    LOG.debug(">>>>> visit - begin");

    LOG.debug("VISIT"
            + ".\n version="    + version
            + ";\n access="     + access
            + ";\n name="       + name
            + ";\n signature="  + signature
            + ";\n superName="  + superName
            + ";\n interfaces=" + interfaces);

    setInternalClassName(name);

    super.visit(version, access, name, signature, superName, interfaces);
  }

  /**
   * @param access        the field's access flags (see Opcodes).
   *                    This parameter also indicates if the field is synthetic
   *                    and/or deprecated.
   * @param name        the field's name.
   * @param desc        the field's descriptor (see Type).
   * @param signature    the field's signature. May be null if the field's type
   *                    does not use generic types.
   * @param value        the field's initial value.
   *                    This parameter, which may be null if the field does not
   *                    have an initial value, must be an Integer, a Float,
   *                    a Long, a Double or a String (for int, float, long or
   *                    String fields respectively).
   *                    This parameter is only used for static fields.
   *                    Its value is IGNORED for non static fields,
   *                    which must be initialized through bytecode
   *                    instructions in constructors or methods.
   *
   * @return    a visitor to visit field annotations and attributes,
   *             or null if this class visitor is not interested
   *             in visiting these annotations and attributes.
   */
  @Override
  public FieldVisitor visitField(int access,
                                 String name,
                                 String desc,
                                 String signature,
                                 Object value) {

    LOG.debug(">>>>> visitField - begin. access="    + access
                                    + "; name="      + name
                                    + "; desc="      + desc
                                    + "; signature=" + signature
                                    + "; value="     + value);

    int __arrayFieldAccess = Opcodes.ACC_PRIVATE + Opcodes.ACC_STATIC;
    String __arrayFieldDesc = "[L" + getInternalClassName() + ";";

    if ("__array".equals(name) 
      && (__arrayFieldAccess == access) && (__arrayFieldDesc.equals(desc))) {

      LOG.debug("The class " + getInternalClassName() + " is a CorbaEnum.");
      setCorbaEnum(true);

    } else {
      checkLabel(access, name, desc);
    }

    return super.visitField(access, name, desc, signature, value);
  }

  /**
   * This method is used to collect the corba enum labels.
   *
   * @param access  The modifiers.
   * @param name    The field name.
   * @param desc    The data type of the field.
   *
   */
  protected void checkLabel(int access, String name, String desc) {

    int labelAccess = Opcodes.ACC_PUBLIC
                    + Opcodes.ACC_STATIC
                    + Opcodes.ACC_FINAL;

    String labelDesc = "L" + getInternalClassName() + ";";

    if (labelAccess == access && labelDesc.equals(desc)) {
      LOG.debug("The field " + name + " is a label of the enumeration.");
      getEnumLabelList().add(name);
    }

  }

  /**
   * A getter.
   * @return  The current value.
   */
  public List<String> getEnumLabelList() {
    return enumLabelList;
  }

  /**
   * A setter.
   * @param   val   The new value
   */
  public void setEnumLabelList(List<String> val) {
    this.enumLabelList = val;
  }

  /**
   * A getter.
   * @return  The current value.
   */
  public boolean isCorbaEnum() {
    return corbaEnum;
  }

  /**
   * A setter.
   * @param   val   The new value
   */
  public void setCorbaEnum(boolean val) {
    this.corbaEnum = val;
  }

  /**
   * A getter.
   * @return  The current value.
   */
  public String getInternalClassName() {
    return internalClassName;
  }

  /**
   * A setter.
   * @param   val   The new value
   */
  public void setInternalClassName(String val) {
    this.internalClassName = val;
  }

}
