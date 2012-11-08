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
import it.imolinfo.jbi4corba.exception.Jbi4CorbaRuntimeException;
import it.imolinfo.jbi4corba.webservice.generator.InterfaceType;
import it.imolinfo.jbi4corba.webservice.generator.Util;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.util.TraceMethodVisitor;

/**
 * This class is used to append (at the default constructor)
 * the code to initialize some fields.
 */
public class AppenderMethodVisitor extends TraceMethodVisitor {

  /**
   * Logger.
   */
  private static final Logger LOG
          = LoggerFactory.getLogger(AppenderMethodVisitor.class);

 /**
  * The fields to initialize. (key = fieldName; value = fieldType)
  */
  private Map<String, String> mapOfFields = new HashMap<String, String>();

 /**
  * The name of the class.
  */
  private String className = null;

  // Utility Object
  private ByteCodeManipulationUtil bcmUtil = new ByteCodeManipulationUtil();
  
  private Map<String, InterfaceType> allIntTypes;

 /**
  * Constructor.
  *
  * @param    mv                 The method visitor.
  * @param    mapOfFields        The fields to initialize.
  * @param    className          The name of the class.
  * @param    allInterfaceTypes  All the Interface type
  */
  public AppenderMethodVisitor(MethodVisitor mv,
                              Map<String, String> mapOfFields,
                              String className) {
    super(mv);

    this.mapOfFields = mapOfFields;
    this.className = className;
    
  }

  /**
   * This method appends (before Opcodes.RETURN) the initialization code of
   * every fields.
   *
   * @param    opcode    @see org.objectweb.asm.Opcodes
   */
  public void visitInsn(int opcode) {
    LOG.debug(">>>>> AppenderMethodVisitor.visitInsn - begin");
    
    if (Opcodes.RETURN == opcode) {
      Iterator<String> i = mapOfFields.keySet().iterator();
      while (i.hasNext()) {
        String k = i.next();
        String v = mapOfFields.get(k);

        LOG.debug("mapOfFields[" + k + "]=" + v);

        initField(k, v, this);
      }
    }
    LOG.debug("<<<<< AppenderMethodVisitor.visitInsn - end");
    super.visitInsn(opcode);
  }

 
	
  
  /**
   * @param fieldName  The field name
   * @param fieldType  The field type
   * @param mv         The method visitor
   */
  private void initField(String fieldName, String fieldType, MethodVisitor mv) {
    LOG.debug(">>>>> initField - begin");

    LOG.debug("fieldName=" + fieldName + "; fieldType=" + fieldType
            + "; Type.getType(fieldType)=" + Type.getType(fieldType));

    String jtype = Type.getType(fieldType).getClassName();

    if (! jtype.endsWith("[]")) {
        LOG.debug(jtype + " is NOT an array ... "
                + "the variable will not be initialized.");
        return;
    }
    // else
    LOG.debug(jtype + " is an ARRAY.");

    Label label = new Label();
    mv.visitLabel(label);
    mv.visitLineNumber(5, label);
    mv.visitVarInsn(Opcodes.ALOAD, 0);
    mv.visitInsn(Opcodes.ICONST_0);

    String obj = jtype.substring(0, jtype.length() - 2);
    if (isAnObject(jtype)) {

      mv.visitTypeInsn(Opcodes.ANEWARRAY, replaceDotWithSlash(obj));

    } else {
      mv.visitIntInsn(Opcodes.NEWARRAY, getOpcode(obj));
    }

    // x = y
    mv.visitFieldInsn(Opcodes.PUTFIELD,
                      bcmUtil.bytecodeClassName(className),
                      fieldName,
                      fieldType);

    LOG.debug("<<<<< initField - end");
  }

  /**
   * @param    jtype    The data type to inspect.
   *
   * @return    The Opcodes of the java primitive associated to jtype.
   *
   * @see        org.objectweb.asm.Opcodes
   */
  private int getOpcode(String jtype) {
    if (boolean.class.getCanonicalName().equals(jtype)) {
      return Opcodes.T_BOOLEAN;
    } else if (byte.class.getCanonicalName().equals(jtype)) {
      return Opcodes.T_BYTE;
    } else if (char.class.getCanonicalName().equals(jtype)) {
      return Opcodes.T_CHAR;
    } else if (double.class.getCanonicalName().equals(jtype)) {
      return Opcodes.T_DOUBLE;
    } else if (float.class.getCanonicalName().equals(jtype)) {
      return Opcodes.T_FLOAT;
    } else if (int.class.getCanonicalName().equals(jtype)) {
      return Opcodes.T_INT;
    } else if (long.class.getCanonicalName().equals(jtype)) {
      return Opcodes.T_LONG;
    } else if (short.class.getCanonicalName().equals(jtype)) {
      return Opcodes.T_SHORT;
    }

    LOG.error("CRB000600_Dont_match_any_primitive_java_types", jtype);
    throw new Jbi4CorbaRuntimeException(
            "CRB000600_Dont_match_any_primitive_java_types",
            new Object[] { jtype }, null);
  }

  /**
   * This method replaces the dot with a '/'.
   *
   * @param    s    The working path.
   *
   * @return    The new string.
   */
  private String replaceDotWithSlash(String s) {
    LOG.debug("replaceDotWithSlash. the input is " + s);
    if (s == null) {
      LOG.debug("replaceDotWithSlash. "
              + "the input is null. returning empty String");
      return "";
    }
    // else
    if ("".equals(s)) {
      LOG.debug("replaceDotWithSlash. "
              + "the input is an empty String. returning empty String");
      return "";
    }
    // else
    String res = s.replaceAll("\\.", "/");
    LOG.debug("replaceDotWithSlash.The input is " + s + " returning " + res);
    return res;
  }


 /**
  * @param    jtype    The data type to inspect.
  * @return    false, if jtype doesn't contain any dots;
  *             true, if jtype contains at least a dot.
  */
  private boolean isAnObject(String jtype) {
    if (jtype.indexOf('.') == -1) {
      LOG.debug(jtype + " is a PRIMITIVE");
      return false;
    }
    // else
    LOG.debug(jtype + " is an OBJECT.");
    return true;
  }

}
