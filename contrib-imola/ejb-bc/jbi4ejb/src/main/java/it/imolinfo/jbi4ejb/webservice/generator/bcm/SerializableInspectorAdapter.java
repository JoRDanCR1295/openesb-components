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
import it.imolinfo.jbi4ejb.webservice.generator.ClassMetaInfo;

import java.util.HashSet;
import java.util.Set;

import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Type;

/**
 * Finds if a class implements the Serializable and gets the interface serial
 * version UIDs, putting all the informtaion a <code>ClassMetaInfo</code> instance.
 */
public class SerializableInspectorAdapter extends ClassAdapter {

    /**
     * The internal name of the java interface 'Serializable'.
     */
    public static final String INTERNAL_NAME_OF_SERIALIZABLE
    = "java/io/Serializable";   

    /**
     * The 'serialVersionUID' field.
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
    private ClassMetaInfo classMetaInfo = new ClassMetaInfo();

    
    /**
     * Instantiates a new serializable inspector adapter.
     * 
     * @param cv The <code>ClassVisitor</code>
     */
    public SerializableInspectorAdapter(ClassVisitor cv) {
        super(cv);
    }

    
    /**
     * Tests if the class implements serializable.
     * 
     * @param version
     *          The version
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
     * If find the serialVersionUID field, gets the value.
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
     * @return
     *          The FieldVisitor
     */
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
     * Checks if the field is a serial version UID field.
     * 
     * @param name
     *          The field name
     * @param desc
     *          The field descriptor
     * 
     * @return true, if successful
     */
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
     * Tests if Serializable is between the interfaces.
     * 
     * @param interfaces
     *          The interfaaces to test
     * 
     * @return true, if successful
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
     * Gets the class meta info.
     * 
     * @return the class meta info
     */
    public ClassMetaInfo getClassMetaInfo() {
        return classMetaInfo;
    }

    /**
     * Sets the class meta info.
     * 
     * @param classMetaInfo
     *            the new class meta info
     */
    public void setClassMetaInfo(ClassMetaInfo classMetaInfo) {
        this.classMetaInfo = classMetaInfo;
    }

}
