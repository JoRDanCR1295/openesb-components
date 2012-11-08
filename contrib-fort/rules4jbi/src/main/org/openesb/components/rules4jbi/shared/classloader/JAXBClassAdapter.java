/*
 * @(#)JAXBClassAdapter.java        $Revision: 1.1 $ $Date: 2008/12/17 23:24:43 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.shared.classloader;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassAdapter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * A class visitor that annotates the visiting class with JAXB annotations.
 * <p>
 * If the visiting class already contains any of the JAXB annotations like
 * <code>@XmlRootElement</code> or <code>@XmlType</code>, the class is left
 * untouched. Otherwise, the these annotations will be added, with
 * <code>namespace</code> attribute derived from the package name of the class,
 * along with the <code>@XmlAccessorType</code> annotation with the value
 * <code>XmlAccessType.FIELD</code>. Subsequently, every non-transient, non-static
 * field of the class will be annotated with the <code>@XmlElement</code> annotation
 * with the namespace attribute as mentioned above. For fields of type <code>String</code>,
 * the attributes <code>required</code> with the value <code>Boolean.TRUE</code> will be
 * added as well.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/12/17 23:24:43 $
 * 
 * @see org.objectweb.asm.ClassVisitor
 * @see org.openesb.components.rules4jbi.shared.classloader.JAXBFieldVisitor
 * @see org.openesb.components.rules4jbi.shared.classloader.NamespaceGenerator
 * @since 0.4
 */
final class JAXBClassAdapter extends ClassAdapter {

    /**
     * Denotes that transformed class containes any of the JAXB annotations;
     * either because the original class had one, or because we have alredy
     * added them.
     */
    private boolean isJAXBAnnotationPresent = false;
    
    /** Denotes that the original class already contained any of the JAXB annotations. */
    private boolean foundExistingJAXBAnnotation = false;
    
    /** Namespace derived from the package name of the visiting class. */
    private String namespace = "";
    
    public JAXBClassAdapter(final ClassVisitor cv) {
        super(cv);
    }
    
    @Override
    public void visit(int version, int access, String name, String signature, String superName,
            String[] interfaces)
    {
        namespace = NamespaceGenerator.namespaceForClass(name);
        
        /*
         * We need to upgrade the class version in case it was less than 1.5,
         * because the JVM would ignore annotations otherwise.
         */
        int v = (version & 0xFF) < Opcodes.V1_5 ? Opcodes.V1_5 : version;
        
        cv.visit(v, access, name, signature, superName, interfaces);
    }

    @Override
    public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
        
        if (visible && (Descriptor.XML_ROOT_ELEMENT.equals(desc)
                || Descriptor.XML_TYPE.equals(desc)
                || Descriptor.XML_ACCESSOR_TYPE.equals(desc)))
        {
            isJAXBAnnotationPresent = true;
            foundExistingJAXBAnnotation = true;
        }
        
        return cv.visitAnnotation(desc, visible);
    }
    
    @Override
    public FieldVisitor visitField(int access, String name, String desc, String signature, Object value) {
        addJAXBAnnotations();
        
        if (foundExistingJAXBAnnotation
                || ((access & Opcodes.ACC_STATIC) != 0)
                || ((access & Opcodes.ACC_TRANSIENT) != 0))
        {
            return cv.visitField(access, name, desc, signature, value);
            
        } else {
            return new JAXBFieldVisitor(
                    cv.visitField(access, name, desc, signature, value), namespace, Descriptor.STRING.equals(desc));
        }
    }
    
    @Override
    public void visitInnerClass(String name, String outerName, String innerName, int access) {
        addJAXBAnnotations();
        
        cv.visitInnerClass(name, outerName, innerName, access);
    }


    @Override
    public MethodVisitor visitMethod(int access, String name, String desc, String signature,
            String[] exceptions)
    {
        addJAXBAnnotations();
        
        return cv.visitMethod(access, name, desc, signature, exceptions);
    }

    @Override
    public void visitEnd() {
        addJAXBAnnotations();
        
        cv.visitEnd();
    }

    private void addJAXBAnnotations() {
        if (!isJAXBAnnotationPresent) {
            addJAXBAnnotationWithNamespace(Descriptor.XML_ROOT_ELEMENT);
            addJAXBAnnotationWithNamespace(Descriptor.XML_TYPE);
            addXmlAccessorTypeAnnotation();
            
            isJAXBAnnotationPresent = true;
        }
    }
    
    private void addJAXBAnnotationWithNamespace(final String desc) {
        final AnnotationVisitor av = cv.visitAnnotation(desc, true);

        if (av != null) {
            av.visit("namespace", namespace);
            av.visitEnd();
        }
    }

    private void addXmlAccessorTypeAnnotation() {
        final AnnotationVisitor av = cv.visitAnnotation(Descriptor.XML_ACCESSOR_TYPE, true);

        if (av != null) {
            av.visitEnum("value", Descriptor.XML_ACCESS_TYPE, "FIELD");
            av.visitEnd();
        }
    }
}
