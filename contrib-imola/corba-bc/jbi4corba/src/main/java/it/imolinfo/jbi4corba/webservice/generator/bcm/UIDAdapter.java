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

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Opcodes;

/**
 * bytecode manipulation.
 *
 * Add the serialVersionUID.
 */
public class UIDAdapter extends ClassAdapter {

    /**
     * Logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(UIDAdapter.class);

    /**
     * The ClassWriter.
     */
    protected ClassWriter classWriter = null;

    /**
     * The new serialVersionUID of the class.
     * If this value is null then no changes are performed;
     */
    protected Long newSerialVersionUID = null;


    /**
    * The adapater used to manipulate the code.
    *
    * @param cv      The ClassVisitor used in this object.
    * @param cw      The ClassWriter used in this object.
    * @param uid     The new serialVersionUID.
    */
    public UIDAdapter(ClassVisitor cv,
                      ClassWriter cw,
                      Long uid) {
    super(cv);

    classWriter = cw;
    newSerialVersionUID = uid;

    LOG.debug("new UIDAdapter; ClassVisitor=" + cv
                + "; ClassWriter=" + cw
                + "; newSerialVersionUID=" + uid);
    }


    /**
     * This method append the serialVersionUID to the class if the attribute
     * newSerialVersionUID is not null.
     */
    @Override
    public void visitEnd() {

      if (newSerialVersionUID != null) {
        LOG.debug("addSerialVersionUid. uid=" + newSerialVersionUID
            + "; classWriter=" + classWriter);

        FieldVisitor fieldVisitor = super.visitField(
            Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC,
            "serialVersionUID",
            "J",
            null,
            newSerialVersionUID);

        fieldVisitor.visitEnd();

      }

      super.visitEnd();
    }

}
