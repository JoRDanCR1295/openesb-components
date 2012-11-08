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

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Opcodes;

/**
 * bytecode manipulation.
 *
 * For each Object visited we modify this bytecode:
 *
 * 1) remove the keyword 'abstract' in the class declaration.
 */
public class WsdlToCorbaAdapter extends ClassAdapter {

    /**
     * Logger.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(WsdlToCorbaAdapter.class);


    protected String classesDirName = null;
    protected String className = null;
    protected ClassWriter classWriter = null;

    protected Map<String, String> mapOfFields = new HashMap<String, String>();

    /**
     * ByteCode Manipulation Utility.
     */
    private ByteCodeManipulationUtil bcmUtil = new ByteCodeManipulationUtil();


    /**
    * The adapater used to manipulate the code.
    *
    * @param cv      The ClassVisitor used in this object.
    * @param cw      The ClassWriter used in this object.
    * @param cn      The class name to manipulate.
    * @param classes The clases used in this object.
    */
    public WsdlToCorbaAdapter(ClassVisitor cv,
                              ClassWriter cw,
                              String cn,
                              String classes) {
    super(cv);

    classesDirName = classes;
    classWriter = cw;
    className = cn;

    LOG.debug("new WsdlToCorbaAdapter; ClassVisitor=" + cv
                + "; ClassWriter=" + cw
                + "; ClassName=" + cn);
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
                + ";\n interfaces=" + interfaces
                + ".\n abstract-code=" + Opcodes.ACC_ABSTRACT
                + ".\n (Opcodes.ACC_ABSTRACT & access)="
                + (Opcodes.ACC_ABSTRACT & access)
                + ".\n access & (~ Opcodes.ACC_ABSTRACT)="
                + (access & (~ Opcodes.ACC_ABSTRACT)));


        //access = Opcodes.ACC_PUBLIC;

        LOG.debug("This class (" + name + ") was an abstract "
                + "... now is a concrete class.");

        LOG.debug("<<<<< visit - end");
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
      public FieldVisitor visitField(
              int access,
              String name,
              String desc,
              String signature,
              Object value) {
    
          LOG.debug(">>>>> visitField - begin:" + desc);
    
          LOG.debug("visitField. access="    + access
                            + "; name="      + name
                            + "; desc="      + desc
                            + "; signature=" + signature
                            + "; value="     + value);
    
          if (Opcodes.ACC_PUBLIC == access || Opcodes.ACC_PROTECTED == access) {
//              bcmUtil.createSetter(classWriter, className, name, desc);
//              bcmUtil.createGetter(classWriter, className, name, desc);
    
              mapOfFields.put(name, desc);
          }
          LOG.debug("<<<<< visitField - end");
    
          //return super.visitField(access, name, desc, signature, value);
          return super.visitField(Opcodes.ACC_PROTECTED, name, desc, signature, value);
      }

    /**
     * XXX javadoc.
     */
    @Override
    public void visitEnd() {

      for (String name : mapOfFields.keySet()) {
        String desc = mapOfFields.get(name);

        
        bcmUtil.createSetter(classWriter,getClassNameWithoutClassesDir(), name, desc);
        bcmUtil.createGetter(classWriter,getClassNameWithoutClassesDir(), name, desc, false);
      }

      bcmUtil.createToString(classWriter);
      bcmUtil.createEquals(classWriter);

      super.visitEnd();
    }

    /**
     * @return  The return
     */
    public String getClassNameWithoutClassesDir() {
      String c = className.substring(
        classesDirName.length() + 1,
        className.length());

      LOG.debug("###### getClassNameWithoutClassesDir=" + c);
      return c;
    }

}
